NODE
{
    Node {
        id: "a",
    },
    Node {
        id: "b",
    },
    Node {
        id: "c",
    },
}

INCOMING
{
    Node {
        id: "b",
    }: {
        Node {
            id: "a",
        },
        Node {
            id: "init",
        },
    },
    Node {
        id: "a",
    }: {
        Node {
            id: "init",
        },
        Node {
            id: "a",
        },
    },
    Node {
        id: "c",
    }: {
        Node {
            id: "c",
        },
        Node {
            id: "b",
        },
    },
}

NOW
{
    Node {
        id: "c",
    }: {},
    Node {
        id: "b",
    }: {
        U(
            Literal(
                "p",
            ),
            Literal(
                "q",
            ),
        ),
    },
    Node {
        id: "a",
    }: {
        U(
            Literal(
                "p",
            ),
            Literal(
                "q",
            ),
        ),
    },
}
