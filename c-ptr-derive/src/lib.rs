use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, DeriveInput, Data, Fields, Type};

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
        
        quote! {
            for item in <#field_type>::type_desc() {
                desc.push(TypeInfo {
                    offset: offset_of!(Self, #field_name) + item.offset,
                    ty: item.ty,
                    name: item.name,
                });
            }
        }
    });

    let expanded = quote! {
        impl TypeDesc for #name {
            fn type_desc() -> Vec<TypeInfo> {
                let mut desc = vec![TypeInfo {
                    offset: 0,
                    ty: std::any::TypeId::of::<Self>(),
                    name: std::any::type_name::<Self>(),
                }];

                #(#field_descs)*

                desc
            }
        }
    };

    TokenStream::from(expanded)
}