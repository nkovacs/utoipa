use std::{borrow::Cow, fmt::Debug, iter};

use proc_macro2::Ident;
use proc_macro_error::{abort, abort_call_site};
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Attribute, GenericArgument, PathArguments, PathSegment, Type, TypePath,
};

use crate::{component_type::ComponentType, Deprecated};

pub mod into_params;

pub mod component;

/// Find `#[deprecated]` attribute from given attributes. Typically derive type attributes
/// or field attributes of struct.
fn get_deprecated(attributes: &[Attribute]) -> Option<Deprecated> {
    attributes.iter().find_map(|attribute| {
        if *attribute.path.get_ident().unwrap() == "deprecated" {
            Some(Deprecated::True)
        } else {
            None
        }
    })
}

pub enum ComponentPart<'a> {
    Singular(ComponentPartValue<'a>),
    Tuple(&'a mut dyn Iterator<Item = ComponentPartValue<'a>>),
}

#[derive(PartialEq)]
#[cfg_attr(feature = "debug", derive(Debug))]
/// Linked list of implementing types of a field in a struct.
pub struct ComponentPartValue<'a> {
    pub path: Cow<'a, TypePath>,
    pub value_type: ValueType,
    pub generic_type: Option<GenericType>,
    pub children: Option<Vec<ComponentPartValue<'a>>>,
}

impl<'a> ComponentPartValue<'a> {
    pub fn from_type(ty: &'a Type) -> &'a mut dyn Iterator<Item = ComponentPartValue> {
        &mut ComponentPartValue::from_type_path(Self::get_type_paths(ty).as_mut())
    }

    fn get_type_paths(ty: &'a Type) -> Box<dyn Iterator<Item = &'a TypePath> + 'a> {
        dbg!("get type path", &ty);
        match ty {
            Type::Path(path) => Box::new(iter::once(path)),
            Type::Reference(reference) => Self::get_type_paths(reference.elem.as_ref()),
            Type::Group(group) => Self::get_type_paths(group.elem.as_ref()),
            Type::Tuple(tuple) => {
                Box::new(tuple.elems.iter().flat_map(Self::get_type_paths))
            },
            _ => abort_call_site!(
                "unexpected type in component part get type path, expected one of: Path, Reference, Group, Tuple"
            ),
        }
    }

    /// Creates a [`ComponentPart`] from a [`syn::TypePath`].
    fn from_type_path(
        path: &'a mut (dyn Iterator<Item = &'a TypePath>),
    ) -> impl Iterator<Item = ComponentPartValue<'a>> + 'a {
        // there will always be one segment at least
        // (i64, HashMap<String, bool>)
        // (i64, HashMap<(i64, HashMap<String, bool>), bool>)
        // how to represent type above in ComponentPart??
        // how to distinct the boundary between tuples and singular types???

        let s = path.into_iter().count();

        path.into_iter().map(|path| {
            let last_segment = path
                .path
                .segments
                .last()
                .expect("at least one segment within path in ComponentPart::from_type_path");

            if last_segment.arguments.is_empty() {
                Self::convert(Cow::Borrowed(path), last_segment)
            } else {
                Self::resolve_component_type(Cow::Borrowed(path), last_segment)
            }
        })
    }

    // Only when type is a generic type we get to this function.
    fn resolve_component_type(
        path: Cow<'a, TypePath>,
        last_segment: &'a PathSegment,
    ) -> ComponentPartValue<'a> {
        let mut generic_component_type = ComponentPartValue::convert(path, last_segment);

        let generic_types = match &last_segment.arguments {
            PathArguments::AngleBracketed(angle_bracketed_args) => {
                // if all type arguments are lifetimes we ignore the generic type
                if angle_bracketed_args
                    .args
                    .iter()
                    .all(|arg| matches!(arg, GenericArgument::Lifetime(_)))
                {
                    None
                } else {
                    Some(angle_bracketed_args.args.iter()
                        .filter(|arg| matches!(arg, GenericArgument::Lifetime(_)))
                        .map(|arg| {
                            match arg {
                                GenericArgument::Type(ty) => ty,
                                _ => abort!(arg, "unexpected GenericArgument, expected GenericArgument::Type in ComponentPart")
                            }

                        }))
                }
            }
            _ => abort!(
                last_segment.ident,
                "unexpected path argument, expected angle bracketed path argument"
            ),
        };

        generic_component_type.children = generic_types
            .map(|generic_types| generic_types.flat_map(ComponentPartValue::from_type))
            .map(|arg| arg.collect());

        generic_component_type
    }

    fn convert(path: Cow<'a, TypePath>, last_segment: &'a PathSegment) -> ComponentPartValue<'a> {
        let generic_type = ComponentPartValue::get_generic(last_segment);
        let is_primitive = ComponentType(&*path).is_primitive();

        Self {
            path,
            value_type: if is_primitive {
                ValueType::Primitive
            } else {
                ValueType::Object
            },
            generic_type,
            children: None,
        }
    }

    fn get_generic(segment: &PathSegment) -> Option<GenericType> {
        match &*segment.ident.to_string() {
            "HashMap" | "Map" | "BTreeMap" => Some(GenericType::Map),
            "Vec" => Some(GenericType::Vec),
            "Option" => Some(GenericType::Option),
            "Cow" => Some(GenericType::Cow),
            "Box" => Some(GenericType::Box),
            "RefCell" => Some(GenericType::RefCell),
            _ => None,
        }
    }

    fn find_mut_by_ident(&mut self, ident: &'a Ident) -> Option<&mut Self> {
        match self.generic_type {
            Some(GenericType::Map) => None,
            Some(GenericType::Vec)
            | Some(GenericType::Option)
            | Some(GenericType::Cow)
            | Some(GenericType::Box)
            | Some(GenericType::RefCell) => {
                self.children
                    .as_mut()
                    .unwrap()
                    .into_iter()
                    .find_map(|child| Self::find_mut_by_ident(child, ident))
                // Self::find_mut_by_ident(self.child.as_mut().unwrap().as_mut(), ident)
            }
            None => {
                if ident.to_token_stream().to_string() == self.path.to_token_stream().to_string() {
                    Some(self)
                } else {
                    None
                }
            }
        }
    }

    fn update_path(&mut self, ident: &'a Ident) {
        self.path = Cow::Owned(TypePath {
            qself: None,
            path: syn::Path::from(ident.clone()),
        })
    }

    /// `Any` virtual type is used when generic object is required in OpenAPI spec. Typically used
    /// with `value_type` attribute to hinder the actual type.
    fn is_any(&self) -> bool {
        &*self.path.to_token_stream().to_string() == "Any"
    }

    /// Check whether [`ComponentPart`]'s [`syn::TypePath`] is a given type as [`str`].
    pub fn is(&self, s: &str) -> bool {
        let mut is = self
            .path
            .path
            .segments
            .last()
            .expect("at least one segment in ComponentPart path")
            .ident
            == s;
        if let Some(ref child) = self.children {
            is = is || child.iter().any(|child| child.is(s))
        }

        is
    }
}

impl<'a> AsMut<ComponentPartValue<'a>> for ComponentPartValue<'a> {
    fn as_mut(&mut self) -> &mut ComponentPartValue<'a> {
        self
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone, Copy, PartialEq)]
pub enum ValueType {
    Primitive,
    Object,
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(PartialEq, Clone, Copy)]
pub enum GenericType {
    Vec,
    Map,
    Option,
    Cow,
    Box,
    RefCell,
}

/// Wrapper for [`syn::Type`] which will be resolved to [`ComponentPart`].
/// This used in `value_type` attribute to override the original field type of a struct.
#[cfg_attr(feature = "debug", derive(Debug))]
struct TypeToken(Type);

impl TypeToken {
    /// Get the [`ComponentPart`] of the [`syn::Type`].
    fn get_component_part(&self) -> &mut dyn Iterator<Item = ComponentPartValue<'_>> {
        ComponentPartValue::from_type(&self.0)
    }
}

impl Parse for TypeToken {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(input.parse::<syn::Type>()?))
    }
}

pub mod serde {
    //! Provides serde related features parsing serde attributes from types.

    use std::str::FromStr;

    use proc_macro2::{Ident, Span, TokenTree};
    use proc_macro_error::ResultExt;
    use syn::{buffer::Cursor, Attribute, Error};

    #[inline]
    fn parse_next_lit_str(next: Cursor) -> Option<(String, Span)> {
        match next.token_tree() {
            Some((tt, next)) => match tt {
                TokenTree::Punct(punct) if punct.as_char() == '=' => parse_next_lit_str(next),
                TokenTree::Literal(literal) => {
                    Some((literal.to_string().replace('\"', ""), literal.span()))
                }
                _ => None,
            },
            _ => None,
        }
    }

    #[derive(Default)]
    #[cfg_attr(feature = "debug", derive(Debug))]
    pub struct SerdeValue {
        pub skip: Option<bool>,
        pub rename: Option<String>,
    }

    impl SerdeValue {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut value = Self::default();

            input.step(|cursor| {
                let mut rest = *cursor;
                while let Some((tt, next)) = rest.token_tree() {
                    match tt {
                        TokenTree::Ident(ident) if ident == "skip" => value.skip = Some(true),
                        TokenTree::Ident(ident) if ident == "rename" => {
                            if let Some((literal, _)) = parse_next_lit_str(next) {
                                value.rename = Some(literal)
                            };
                        }
                        _ => (),
                    }

                    rest = next;
                }
                Ok(((), rest))
            })?;

            Ok(value)
        }
    }

    /// Attributes defined within a `#[serde(...)]` container attribute.
    #[derive(Default)]
    #[cfg_attr(feature = "debug", derive(Debug))]
    pub struct SerdeContainer {
        pub rename_all: Option<RenameRule>,
        pub tag: Option<String>,
    }

    impl SerdeContainer {
        /// Parse a single serde attribute, currently `rename_all = ...` and `tag = ...` attributes
        /// are supported.
        fn parse_attribute(&mut self, ident: Ident, next: Cursor) -> syn::Result<()> {
            match ident.to_string().as_str() {
                "rename_all" => {
                    if let Some((literal, span)) = parse_next_lit_str(next) {
                        self.rename_all = Some(
                            literal
                                .parse::<RenameRule>()
                                .map_err(|error| Error::new(span, error.to_string()))?,
                        );
                    };
                }
                "tag" => {
                    if let Some((literal, _span)) = parse_next_lit_str(next) {
                        self.tag = Some(literal)
                    }
                }
                _ => {}
            }
            Ok(())
        }

        /// Parse the attributes inside a `#[serde(...)]` container attribute.
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut container = Self::default();

            input.step(|cursor| {
                let mut rest = *cursor;
                while let Some((tt, next)) = rest.token_tree() {
                    if let TokenTree::Ident(ident) = tt {
                        container.parse_attribute(ident, next)?
                    }

                    rest = next;
                }
                Ok(((), rest))
            })?;

            Ok(container)
        }
    }

    pub fn parse_value(attributes: &[Attribute]) -> Option<SerdeValue> {
        attributes
            .iter()
            .find(|attribute| attribute.path.is_ident("serde"))
            .map(|serde_attribute| {
                serde_attribute
                    .parse_args_with(SerdeValue::parse)
                    .unwrap_or_abort()
            })
    }

    pub fn parse_container(attributes: &[Attribute]) -> Option<SerdeContainer> {
        attributes
            .iter()
            .find(|attribute| attribute.path.is_ident("serde"))
            .map(|serde_attribute| {
                serde_attribute
                    .parse_args_with(SerdeContainer::parse)
                    .unwrap_or_abort()
            })
    }

    #[cfg_attr(feature = "debug", derive(Debug))]
    pub enum RenameRule {
        Lower,
        Upper,
        Camel,
        Snake,
        ScreamingSnake,
        Pascal,
        Kebab,
        ScreamingKebab,
    }

    impl RenameRule {
        pub fn rename(&self, value: &str) -> String {
            match self {
                RenameRule::Lower => value.to_ascii_lowercase(),
                RenameRule::Upper => value.to_ascii_uppercase(),
                RenameRule::Camel => {
                    let mut camel_case = String::new();

                    let mut upper = false;
                    for letter in value.chars() {
                        if letter == '_' {
                            upper = true;
                            continue;
                        }

                        if upper {
                            camel_case.push(letter.to_ascii_uppercase());
                            upper = false;
                        } else {
                            camel_case.push(letter)
                        }
                    }

                    camel_case
                }
                RenameRule::Snake => value.to_string(),
                RenameRule::ScreamingSnake => Self::Snake.rename(value).to_ascii_uppercase(),
                RenameRule::Pascal => {
                    let mut pascal_case = String::from(&value[..1].to_ascii_uppercase());
                    pascal_case.push_str(&Self::Camel.rename(&value[1..]));

                    pascal_case
                }
                RenameRule::Kebab => Self::Snake.rename(value).replace('_', "-"),
                RenameRule::ScreamingKebab => Self::Kebab.rename(value).to_ascii_uppercase(),
            }
        }

        pub fn rename_variant(&self, variant: &str) -> String {
            match self {
                RenameRule::Lower => variant.to_ascii_lowercase(),
                RenameRule::Upper => variant.to_ascii_uppercase(),
                RenameRule::Camel => {
                    let mut snake_case = String::from(&variant[..1].to_ascii_lowercase());
                    snake_case.push_str(&variant[1..]);

                    snake_case
                }
                RenameRule::Snake => {
                    let mut snake_case = String::new();

                    for (index, letter) in variant.char_indices() {
                        if index > 0 && letter.is_uppercase() {
                            snake_case.push('_');
                        }
                        snake_case.push(letter);
                    }

                    snake_case.to_ascii_lowercase()
                }
                RenameRule::ScreamingSnake => {
                    Self::Snake.rename_variant(variant).to_ascii_uppercase()
                }
                RenameRule::Pascal => variant.to_string(),
                RenameRule::Kebab => Self::Snake.rename_variant(variant).replace('_', "-"),
                RenameRule::ScreamingKebab => {
                    Self::Kebab.rename_variant(variant).to_ascii_uppercase()
                }
            }
        }
    }

    impl FromStr for RenameRule {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            [
                ("lowercase", RenameRule::Lower),
                ("UPPERCASE", RenameRule::Upper),
                ("Pascal", RenameRule::Pascal),
                ("camelCase", RenameRule::Camel),
                ("snake_case", RenameRule::Snake),
                ("SCREAMING_SNAKE_CASE", RenameRule::ScreamingSnake),
                ("kebab-case", RenameRule::Kebab),
                ("SCREAMING-KEBAB-CASE", RenameRule::ScreamingKebab),
            ]
            .into_iter()
            .find_map(|(case, rule)| if case == s { Some(rule) } else { None })
            .ok_or_else(|| {
                Error::new(
                    Span::call_site(),
                    r#"unexpected rename rule, expected one of: "lowercase", "UPPERCASE", "Pascal", "camelCase", "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case", "SCREAMING-KEBAB-CASE""#,
                )
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::serde::RenameRule;

    macro_rules! test_rename_rule {
        ( $($case:expr=> $value:literal = $expected:literal)* ) => {
            #[test]
            fn rename_all_rename_rules() {
                $(
                    let value = $case.rename($value);
                    assert_eq!(value, $expected, "expected case: {} => {} != {}", stringify!($case), $value, $expected);
                )*
            }
        };
    }

    macro_rules! test_rename_variant_rule {
        ( $($case:expr=> $value:literal = $expected:literal)* ) => {
            #[test]
            fn rename_all_rename_variant_rules() {
                $(
                    let value = $case.rename_variant($value);
                    assert_eq!(value, $expected, "expected case: {} => {} != {}", stringify!($case), $value, $expected);
                )*
            }
        };
    }

    test_rename_rule! {
        RenameRule::Lower=> "single" = "single"
        RenameRule::Upper=> "single" = "SINGLE"
        RenameRule::Pascal=> "single" = "Single"
        RenameRule::Camel=> "single" = "single"
        RenameRule::Snake=> "single" = "single"
        RenameRule::ScreamingSnake=> "single" = "SINGLE"
        RenameRule::Kebab=> "single" = "single"
        RenameRule::ScreamingKebab=> "single" = "SINGLE"

        RenameRule::Lower=> "multi_value" = "multi_value"
        RenameRule::Upper=> "multi_value" = "MULTI_VALUE"
        RenameRule::Pascal=> "multi_value" = "MultiValue"
        RenameRule::Camel=> "multi_value" = "multiValue"
        RenameRule::Snake=> "multi_value" = "multi_value"
        RenameRule::ScreamingSnake=> "multi_value" = "MULTI_VALUE"
        RenameRule::Kebab=> "multi_value" = "multi-value"
        RenameRule::ScreamingKebab=> "multi_value" = "MULTI-VALUE"
    }

    test_rename_variant_rule! {
        RenameRule::Lower=> "Single" = "single"
        RenameRule::Upper=> "Single" = "SINGLE"
        RenameRule::Pascal=> "Single" = "Single"
        RenameRule::Camel=> "Single" = "single"
        RenameRule::Snake=> "Single" = "single"
        RenameRule::ScreamingSnake=> "Single" = "SINGLE"
        RenameRule::Kebab=> "Single" = "single"
        RenameRule::ScreamingKebab=> "Single" = "SINGLE"

        RenameRule::Lower=> "MultiValue" = "multivalue"
        RenameRule::Upper=> "MultiValue" = "MULTIVALUE"
        RenameRule::Pascal=> "MultiValue" = "MultiValue"
        RenameRule::Camel=> "MultiValue" = "multiValue"
        RenameRule::Snake=> "MultiValue" = "multi_value"
        RenameRule::ScreamingSnake=> "MultiValue" = "MULTI_VALUE"
        RenameRule::Kebab=> "MultiValue" = "multi-value"
        RenameRule::ScreamingKebab=> "MultiValue" = "MULTI-VALUE"
    }

    #[test]
    fn test_serde_rename_rule_from_str() {
        for s in [
            "lowercase",
            "UPPERCASE",
            "Pascal",
            "camelCase",
            "snake_case",
            "SCREAMING_SNAKE_CASE",
            "kebab-case",
            "SCREAMING-KEBAB-CASE",
        ] {
            s.parse::<RenameRule>().unwrap();
        }
    }
}
