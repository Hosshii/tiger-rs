use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Parser},
    Error, LitInt, Result, Token,
};

/// generate_match!(
///     BinOp;NumType; I32,  I64,  F32,  F64;
///     Add;           0x6A, 0x7C, 0x92, 0xA0;
///     Sub;           0x6B, 0x7D, 0x93, 0xA1;
/// );

///     (NumType::I32, BinOp::Add) => 0x6A,
///     (NumType::I64, BinOp::Add) => 0x7C,
///     (NumType::F32, BinOp::Add) => 0x92,
///     (NumType::F64, BinOp::Add) => 0xA0,
///     (NumType::I32, BinOp::Sub) => 0x6B,
///     (NumType::I64, BinOp::Sub) => 0x7D,
///     (NumType::F32, BinOp::Sub) => 0x93,
///     (NumType::F64, BinOp::Sub) => 0xA1,

#[proc_macro]
pub fn generate_match(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    generate_match_impl(tokens.into()).into()
}

fn generate_match_impl(tokens: TokenStream) -> TokenStream {
    generate_match_parse
        .parse2(tokens)
        .unwrap_or_else(Error::into_compile_error)
}

fn generate_match_parse(input: ParseStream) -> Result<TokenStream> {
    let (bin_op, num_type, num_type_variants) = parse_first_row(&input)?;

    let mut match_arms = TokenStream::new();

    while !input.is_empty() {
        let (bin_op_variant, opcodes) = parse_row(&input)?;

        if num_type_variants.len() != opcodes.len() {
            return Err(Error::new(
                bin_op_variant.span(), // TODO: better span
                format!(
                    "num_types.len() != opcodes.len(): {} != {}",
                    num_type_variants.len(),
                    opcodes.len()
                ),
            ));
        }

        for (num_type_variant, opcode) in num_type_variants.iter().zip(opcodes.iter()) {
            match_arms.extend(quote! {
                (#num_type::#num_type_variant, #bin_op::#bin_op_variant) => #opcode,
            });
        }
    }

    Ok(match_arms)
}

fn expect_t<T: Parse>(input: &ParseStream) -> Result<T> {
    match input.parse::<T>() {
        Ok(t) => Ok(t),
        Err(e) => Err(Error::new(
            input.span(),
            format!("expected: {}\nerror: {}", std::any::type_name::<T>(), e),
        )),
    }
}

/// parse like
/// BinOp;NumType; I32,  I64,  F32,  F64;
fn parse_first_row(input: &ParseStream) -> Result<(Ident, Ident, Vec<Ident>)> {
    let bin_op: Ident = expect_t(input)?;
    expect_t::<Token![;]>(input)?;

    let num_type: Ident = expect_t(input)?;
    expect_t::<Token![;]>(input)?;

    let num_types = parse_separated_list(input)?;

    Ok((bin_op, num_type, num_types))
}

/// parse like
/// Add; 0x6A, 0x7C, 0x92, 0xA0;
fn parse_row(input: &ParseStream) -> Result<(Ident, Vec<LitInt>)> {
    let bin_op: Ident = expect_t(input)?;
    expect_t::<Token![;]>(input)?;

    let opcodes = parse_separated_list(input)?;

    Ok((bin_op, opcodes))
}

/// parse like
/// I32,  I64,  F32,  F64;
fn parse_separated_list<T: Parse>(input: &ParseStream) -> Result<Vec<T>> {
    let mut result = Vec::new();
    while !input.peek(Token![;]) {
        result.push(expect_t::<T>(input)?);
        if !input.peek(Token![,]) {
            break;
        }
        expect_t::<Token![,]>(input)?;
    }
    expect_t::<Token![;]>(input)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    enum NumType {
        I32,
        I64,
    }

    enum BinOp {
        Add,
        Sub,
    }

    #[test]
    fn test_generate_match() {
        generate_match_impl(TokenStream::new());
        assert_eq!(
            generate_match_impl(quote! {
                BinOp;NumType;  I32,  I64;
                Add;           0x6A, 0x7C ;
                Sub;           0x6B, 0x7D ;
            })
            .to_string(),
            quote! {
                    (NumType::I32, BinOp::Add) => 0x6A,
                    (NumType::I64, BinOp::Add) => 0x7C,
                    (NumType::I32, BinOp::Sub) => 0x6B,
                    (NumType::I64, BinOp::Sub) => 0x7D,
            }
            .to_string()
        );
    }
}
