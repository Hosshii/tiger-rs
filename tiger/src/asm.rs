use std::fmt::Display;

use crate::{
    common::{Label as CommonLabel, Temp as CommonTemp},
    frame::Frame,
};

type Register = String;
type Label = CommonLabel;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Temp(CommonTemp);

impl Temp {
    pub fn new() -> Self {
        Self(CommonTemp::new())
    }

    pub fn new_with(num: u32) -> Self {
        Self(CommonTemp::new_with(num))
    }

    pub fn to_string<F: Frame>(&self) -> String {
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
}

impl Instruction {
    pub fn to_string<F: Frame>(&self) -> String {
        match self {
            Instruction::Label { assembly, .. } => assembly.clone(),
            Instruction::Operand {
                assembly, dst, src, ..
            } => {
                let mut result = assembly.clone();
                for (idx, dst) in dst.iter().enumerate() {
                    result = result.replace(&format!("'d{}", idx), &dst.to_string::<F>());
                }
                for (idx, dst) in src.iter().enumerate() {
                    result = result.replace(&format!("'s{}", idx), &dst.to_string::<F>());
                }
                assert!(!result.contains("'d") && !result.contains("'s"));
                result
            }
            Instruction::Move { assembly, dst, src } => {
                let mut result = assembly.clone();
                result = result.replace("'d0", &dst.to_string::<F>());
                result = result.replace("'s0", &src.to_string::<F>());
                assert!(!result.contains("'d") && !result.contains("'s"));
                result
            }
        }
    }
}
