use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::{
    common::{Label as CommonLabel, Temp as CommonTemp},
    frame::Frame,
};

type Label = CommonLabel;
pub type Allocation<F> = HashMap<<F as Frame>::Temp, <F as Frame>::Register>;

pub trait TempTrait:
    Hash + Clone + PartialEq + Eq + Debug + Copy + From<CommonTemp> + for<'a> From<&'a CommonTemp>
{
}
pub trait LabelTrait:
    Hash + Clone + PartialEq + Eq + Debug + From<CommonLabel> + for<'a> From<&'a CommonLabel>
{
}

#[derive(Debug, Clone)]
pub enum Instruction<T, L>
where
    T: TempTrait,
    L: LabelTrait,
{
    Operand {
        assembly: String,
        dst: Vec<T>,
        src: Vec<T>,
        jump: Option<Vec<L>>,
    },
    Label {
        assembly: String,
        label: L,
    },
    Move {
        assembly: String,
        dst: T,
        src: T,
    },

    Comment {
        assembly: String,
    },
}

impl<T, L> Instruction<T, L>
where
    T: TempTrait,
    L: LabelTrait,
{
    pub fn to_string<F>(&self, allocation: &Allocation<F>) -> String
    where
        F: Frame<Temp = T, Label = L>,
    {
        match self {
            Instruction::Label { assembly, .. } | Instruction::Comment { assembly } => {
                assembly.clone()
            }
            Instruction::Operand {
                assembly, dst, src, ..
            } => {
                let mut result = assembly.clone();
                for (idx, dst) in dst.iter().enumerate() {
                    result = result.replace(&format!("'d{}", idx), &allocation[dst].to_string());
                }
                for (idx, src) in src.iter().enumerate() {
                    result = result.replace(&format!("'s{}", idx), &allocation[src].to_string());
                }
                assert!(!result.contains("'d") && !result.contains("'s"));
                result
            }
            Instruction::Move { assembly, dst, src } => {
                let mut result = assembly.clone();
                result = result.replace("'d0", &allocation[dst].to_string());
                result = result.replace("'s0", &allocation[src].to_string());
                assert!(!result.contains("'d") && !result.contains("'s"));
                result
            }
        }
    }
}
