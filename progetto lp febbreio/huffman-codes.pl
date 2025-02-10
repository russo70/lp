:- dynamic node/2.
:- dynamic vector/3.
:- dynamic hucodec_encode_file/3.
:- dynamic hucodec_generate_huffman_tree/2.
:- dynamic hucodec_generate_symbol_bits_table/2.
:- dynamic hucode_print_huffman_tree/1.

void_list([]):-!.

one_element_list([_]):-!.

cancella_tutti_nodi :-
    retractall(node(_, _)).



%hucodec_encode

hucodec_encode():-
    !.



%hucodec_generate_huffman_tree

hucodec_generate_huffman_tree(List, Tree):-
    is_list(List),
    not(void_list(List)),
    not(one_element_list(List)),
    create_node(List, Tree),
    !.



create_new_node([[L, N], [L1, N1]]):-
    not(is_list(L)),
   not( is_list(L1)),
    N2 is N + N1,
    assert(node([L, L1],N2)),
    create_vector([[L, N], [L1, N1]]),

    !.

create_new_node([[L, N], [L1, N1]]):-
    is_list(L),
    is_list(L1),
    append(L, L1, Result),
    N2 is N + N1,
    assert(node(Result,N2)),
    create_vector([[L, N], [L1, N1]]),
    !.

create_new_node([[L, N], [L1, N1]]):-
    is_list(L),
   not(is_list(L1)),
    append(L, [L1], Result),
    N2 is N + N1,
    assert(node(Result,N2)),
    create_vector([[L, N], [L1, N1]]),
    !.

create_new_node([[L, N], [L1, N1]]):-
   not(is_list(L)),
   is_list(L1),
    append([L], L1, Result),
    N2 is N + N1,
    assert(node(Result,N2)),
    create_vector([[L, N], [L1, N1]]),
    !.



create_node([[S1,N1],[S2, N2]|List],Node):-
    not(void_list(List)),
    not(one_element_list(List)),
    N1 == N2,
    create_new_node([[S1,N1],[S2, N2]]),
    create_node(List, SNode),
    findall(N, node([S1,S2], N), [N3|_]),
    S = [S1, S2],
    DNode =[S, N3],
    create_new_node([DNode,SNode]),
    DNode = [L1|_],
    SNode = [L2|_],
     append(L1, L2, Result),
    findall(N_1, node(Result, N_1), [N_2|_]),
    Node =[Result, N_2],
    !.

create_node([[S1,N1],[S2, N2]|List],Node):-
    not(void_list(List)),
    not(one_element_list(List)),
    not(N1 == N2),
    create_node([[S2, N2]|List], SNode),
     create_new_node([[S1,N1],SNode]),
    SNode=[List1|_],
     append([S1], List1, Result),
    findall(N, node(Result, N), [N3|_]),
    Node =[Result, N3],
    !.

create_node([[S1,N1],[S2, N2]|List],Node):-
    not(void_list(List)),
    one_element_list(List),
    N1 == N2,
    create_new_node([[S1,N1],[S2, N2]]),
    findall(N, node([S1,S2], N), [N3|_]),
    S = [S1, S2],
    DNode =[S, N3],
    List = [List1|_],
    create_new_node([DNode,List1]),
    DNode = [L1|_],
    List1 = [L2|_],
    append(L1, [L2], Result),
    findall(N_1, node(Result, N_1), [N_2|_]),
    Node =[Result, N_2],

    !.

create_node([[S1,N1],[S2, N2]|List],Node):-
    not(void_list(List)),
    one_element_list(List),
    not(N1 == N2),
    create_node([[S2, N2]|List],DNote ),
    create_new_node([[S1,N1],DNote]),
    DNote = [List1|_],
     append([S1], List1, Result),
    findall(N, node(Result, N), [N3|_]),
    Node =[Result, N3],

    !.

create_node([[S1,N1],[S2, N2]|List], Node):-
    void_list(List),
    create_new_node([[S1,N1],[S2, N2]]),
    findall(N, node([S1,S2], N), [N3|_]),
    S = [S1, S2],
    Node =[S, N3],
    !.

create_vector([[L, _], [L1, _]]):-
    not( is_list(L)),
   not( is_list(L1)),
     assert(vector([L, L1],l,0)),
     assert(vector([L, L1],l1,1)),
    !.

create_vector([[L, _], [L1, _]]):-
    is_list(L),
    is_list(L1),
     append(L, L1, Result),
     assert(vector(Result,L,0)),
     assert(vector(Result,L1,1)),
    !.

create_vector([[L, _], [L1, _]]):-
    is_list(L),
    not(is_list(L1)),
     append(L, [L1], Result),
     assert(vector(Result,L,0)),
     assert(vector(Result,L1,1)),
    !.

create_vector([[L, _], [L1, _]]):-
   not(is_list(L)),
    is_list(L1),
     append([L], L1, Result),
     assert(vector(Result,L,0)),
     assert(vector(Result,L1,1)),
    !.
