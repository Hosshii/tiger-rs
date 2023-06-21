use std::collections::HashMap;

use crate::{
    common::{Label as CommonLabel, Temp as CommonTemp},
    frame::Frame,
};

type Label = CommonLabel;
pub type Allocation<F> = HashMap<Temp, <F as Frame>::Register>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Temp(CommonTemp);

impl Temp {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(CommonTemp::new())
    }

    #[cfg(test)]
    pub fn new_with(num: u32) -> Self {
        Self(CommonTemp::new_with(num))
    }

    pub fn _to_string<F: Frame>(self) -> String {
        F::temp_map()
            .get(&self.0)
            .map(ToString::to_string)
            .unwrap_or_else(|| format!("t{}", self.0))
    }
}

impl From<CommonTemp> for Temp {
    fn from(tmp: CommonTemp) -> Self {
        Self(tmp)
    }
}

impl From<&CommonTemp> for Temp {
    fn from(tmp: &CommonTemp) -> Self {
        Self(*tmp)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Operand {
        assembly: String,
        dst: Vec<Temp>,
        src: Vec<Temp>,
        jump: Option<Vec<Label>>,
    },
    Label {
        assembly: String,
        label: CommonLabel,
    },
    Move {
        assembly: String,
        dst: Temp,
        src: Temp,
    },

    Comment {
        assembly: String,
    },
}

impl Instruction {
    pub fn to_string<F: Frame>(&self, allocation: &Allocation<F>) -> String {
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
