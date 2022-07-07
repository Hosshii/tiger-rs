use std::fmt::Display;

use crate::common::{Label as CommonLabel, Temp as CommonTemp};

type Register = String;
type Label = CommonLabel;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Temp(CommonTemp);

impl Temp {
    pub fn new() -> Self {
        Self(CommonTemp::new())
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
