use crate::expression::LTLExpression;
use crate::buchi::{BuchiNode, Buchi};


#[macro_export]
macro_rules! buchi{
    (
        $(
            $src: ident
                $([$ltl:expr] => $dest: ident)*
        )*
        ===
        init = [$( $init:ident ),*]
        accepting = [$( $accepting_state:ident ),*]
    ) => {{
        let mut __graph = Buchi::new();
        $(
            let mut $src = BuchiNode::new(stringify!($src).to_string());
            $(
                $src.adj.push(
                    BuchiNode {
                        id: stringify!($dest).into(),
                        labels: vec![$ltl],
                        adj: vec![],
                    }
                );
            )*

            __graph.adj_list.push($src.clone());
        )*

        $(__graph.init_states.push($init.clone());)*
        $(__graph.accepting_states.push($accepting_state.clone());)*

        __graph
    }};
}
