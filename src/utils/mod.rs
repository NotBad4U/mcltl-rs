pub mod dot;

#[macro_export]
macro_rules! buchi{
    (
        $(
            $src: ident
                $([$( $ltl:expr ),*] => $dest: ident)*
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
                        labels: vec![$($ltl),*],
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

#[macro_export]
macro_rules! gbuchi{
    (
        $(
            $src: ident
                $([$ltl:expr] => $dest: ident)*
        )*
        ===
        init = [$( $init:ident ),*]
        $(accepting = [$( $accepting_states:expr ),*])*
    ) => {{
        let mut __graph = GeneralBuchi::new();
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
        $($(__graph.accepting_states.push($accepting_states.clone());)*)*

        __graph
    }};
}

#[macro_export]
macro_rules! kripke{
    (
        $(
            $world:ident = [$( $prop:expr),*]
        )*
        ===
        $(
            $src:ident R $dst:ident
        )*
        ===
        init = [$( $init:ident ),*]
    ) => {{
        let mut __kripke = KripkeStructure::new(vec![]);

        $(
            let mut $world = World {
                id: stringify!($world).into(),
                assignement: std::collections::HashMap::new(),
            };
            $(
                $world.assignement.insert($prop.0.into(), $prop.1);
            )*

            __kripke.add_world($world.clone());
        )*

        $(
            __kripke.add_relation($src.clone(), $dst.clone());
        )*

        __kripke.inits = vec![$($init.id.clone(),)*];

        __kripke
    }};
}
