#[cfg(not(test))] extern crate proc_macro;
#[macro_use] extern crate quote;
extern crate syn;
extern crate synstructure;

fn type_desc_derive(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    let body = s.each(|bi| quote!{
        walk(#bi)
    });


    s.gen_impl(quote! {
        gen impl TypeDesc for @Self {
            fn type_desc() -> Vec<TypeInfo> {
                match *self { #body }
            }
        }
    })
}
synstructure::decl_derive!([TypeDesc] => type_desc_derive);
