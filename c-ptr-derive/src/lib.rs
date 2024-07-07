use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, DeriveInput, Data, Fields, Type};

fn is_option_fn_ptr(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(ref args) = segment.arguments {
                    if let Some(syn::GenericArgument::Type(Type::BareFn(_))) = args.args.first() {
                        return true;
                    }
                }
            }
        }
    }
    false
}

#[proc_macro_derive(TypeDesc)]
pub fn derive_type_desc(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let fields = match &input.data {
        Data::Struct(data) => {
            match &data.fields {
                Fields::Named(fields) => &fields.named,
                _ => panic!("TypeDesc can only be derived for structs with named fields"),
            }
        },
        _ => panic!("TypeDesc can only be derived for structs"),
    };

    let field_descs = fields.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;

        if is_option_fn_ptr(field_type) {
            // We can't derive TypeDesc for Option<fn()>, so we output the type info for
            // that field as a single item with the type of the Option itself.
            quote! {
                desc.push(::c_ptr::TypeInfo {
                    offset: std::mem::offset_of!(Self, #field_name),
                    ty: std::any::TypeId::of::<#field_type>(),
                    size: std::mem::size_of::<#field_type>(),
                    name: std::any::type_name::<#field_type>(),
                });
            }
        } else {
            quote! {
                for item in <#field_type>::type_desc() {
                    desc.push(::c_ptr::TypeInfo {
                        offset: std::mem::offset_of!(Self, #field_name) + item.offset,
                        ty: item.ty,
                        size: std::mem::size_of::<#field_type>(),
                        name: item.name,
                    });
                }
            }
        }
    });

    let expanded = quote! {
        impl TypeDesc for #name {
            fn type_desc() -> Vec<::c_ptr::TypeInfo> {
                let mut desc = vec![::c_ptr::TypeInfo {
                    offset: 0,
                    ty: std::any::TypeId::of::<Self>(),
                    size: std::mem::size_of::<Self>(),
                    name: std::any::type_name::<Self>(),
                }];

                #(#field_descs)*

                desc
            }
        }
    };

    TokenStream::from(expanded)
}