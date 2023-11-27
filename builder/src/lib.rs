use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, DeriveInput, Expr, ExprAssign, ExprPath, GenericArgument,
    Lit, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder = format_ident!("{}Builder", name);
    let fields_raw = input.data;
    let fields = parse_fields(&fields_raw, &ParseMode::Option);
    let none_fields = parse_fields(&fields_raw, &ParseMode::None);
    let setters = parse_fields(&fields_raw, &ParseMode::Setter);
    let getters = parse_fields(&fields_raw, &ParseMode::Getter);
    let expanded = quote! {

        pub struct #builder {
            #fields
        }

        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #none_fields
                }
            }
       }

        impl #builder {
            #setters

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #getters
                })
            }
        }
    };

    // eprintln!("TOKENS: {}", expanded);
    TokenStream::from(expanded)
}

fn parse_fields(fields: &syn::Data, parse_mode: &ParseMode) -> proc_macro2::TokenStream {
    let fields = match fields {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(nf) => {
                let recurse = nf.named.iter().map(|f| parse_field(f, parse_mode));

                quote! {
                    #(#recurse)*
                }
            }
            syn::Fields::Unnamed(uf) => {
                let recurse = uf.unnamed.iter().map(|f| parse_field(f, parse_mode));
                quote! {
                    #(#recurse)*
                }
            }
            syn::Fields::Unit => todo!(),
        },
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    };
    fields
}

fn parse_field(f: &syn::Field, as_option: &ParseMode) -> proc_macro2::TokenStream {
    let fname = &f.ident;
    let ty = &f.ty;

    let option_type = option_inner_type(ty, "Option".into());

    match as_option {
        ParseMode::Option => {
            if let Some(inner) = option_type {
                quote_spanned! {f.span()=>
                    #fname: std::option::Option<#inner>,
                }
            } else {
                quote_spanned! {f.span()=>
                    #fname: std::option::Option<#ty>,
                }
            }
        }
        ParseMode::None => {
            if let Ok(Some(each)) = parse_attr(f) {
                quote_spanned! {f.span()=>
                    #fname: std::option::Option::Some(vec![]),
                }
            } else {
                quote_spanned! {f.span()=>
                    #fname: std::option::Option::None,
                }
            }
        }
        ParseMode::Setter => match parse_attr(f) {
            Ok(some_each) => {
                if let Some(each) = some_each {
                    let vec_type = option_inner_type(ty, "Vec".into()).expect("Bad Vec");
                    quote_spanned! {f.span()=>
                        fn #each(&mut self, #each: #vec_type) -> &mut Self {
                            if let Some(ref mut vec) = self.#fname {
                                vec.push(#each);
                            }
                            self
                        }
                    }
                } else {
                    parse_setters(option_type, f, fname, ty)
                }
            }
            Err(str) => {
                let message = str.message;
                quote_spanned! { str.span=>
                    compile_error!(#message);
                }
            }
        },
        ParseMode::Getter => {
            if option_type.is_some() {
                quote_spanned! {f.span()=>
                    #fname : self.#fname.clone(),
                }
            } else {
                quote_spanned! {f.span()=>
                    #fname : self.#fname.clone()
                    .ok_or::<std::boxed::Box<dyn std::error::Error>>
                    (concat!("Value not set !", stringify!(#fname)).into())?,
                }
            }
        }
    }
}

fn parse_setters(
    option_type: Option<&Type>,
    f: &syn::Field,
    fname: &Option<Ident>,
    ty: &Type,
) -> proc_macro2::TokenStream {
    if let Some(inner) = option_type {
        quote_spanned! {f.span()=>
            fn #fname(&mut self, #fname: #inner) -> &mut Self {
                self.#fname = std::option::Option::Some(#fname);
                self
            }
        }
    } else {
        quote_spanned! {f.span()=>
            fn #fname(&mut self, #fname: #ty) -> &mut Self {
                self.#fname = std::option::Option::Some(#fname);
                self
            }
        }
    }
}

fn parse_attr(f: &syn::Field) -> Result<Option<Ident>, BuilderError> {
    let Some(attr) = f.attrs.first() else {
        return Ok(None);
    };
    let args = attr.parse_args::<Expr>().map_err(|_| BuilderError {
        message: "Failed parsing arg".into(),
        span: attr.span(),
    })?;
    let Expr::Assign(ExprAssign { left, right, .. }) = args else {
        return Ok(None);
    };

    let Expr::Path(ExprPath { path, .. }) = *left else {
        return Ok(None);
    };

    if path.is_ident("each") {
        let Expr::Lit(lit) = *right else {
            return Err(BuilderError {
                message: "bad left side".into(),
                span: path.span(),
            });
        };
        let Lit::Str(lit_str) = lit.lit else {
            return Err(BuilderError {
                message: "Bad right side".into(),
                span: lit.lit.span(),
            });
        };

        return lit_str.parse().map_err(|_| BuilderError {
            message: "Bad right side".into(),
            span: lit_str.span(),
        });
    }
    Err(BuilderError {
        message: "expected `builder(each = \"...\")`".into(),
        span: path.span(),
    })
}

struct BuilderError {
    message: String,
    span: Span,
}

fn option_inner_type(ty: &Type, ident: String) -> Option<&Type> {
    let Type::Path(ref s) = ty else { return None };
    let path = s.path.segments.first()?;
    if path.ident != ident {
        return None;
    }
    let PathArguments::AngleBracketed(ref args) = path.arguments else {
        return None;
    };
    let GenericArgument::Type(ref t) = args.args.first()? else {
        return None;
    };
    Some(t)
}

enum ParseMode {
    Option,
    None,
    Setter,
    Getter,
}
