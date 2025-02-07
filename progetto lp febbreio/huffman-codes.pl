:- dynamic node/2.
:- dynamic vector/3.
:- dynamic hucodec_encode_file/3.
:- dynamic hucodec_generate_huffman_tree/2.
:- dynamic hucodec_generate_symbol_bits_table/2.
:- dynamic hucode_print_huffman_tree/1.

void_list([]):-!.

one_element_list([_]):-!.



%hucodec_encode

hucodec_encode():-
    !.



%hucodec_generate_huffman_tree

hucodec_generate_huffman_tree(List, Tree):-
    is_list(List),
    not(void_list(List)),
    not(one_element_list(List)),
    head_node(List):
    Tree=C,
    writeln(C),
    !.



create_new_node([[L, N], [L1, N1]]):-
    number(N),
    not(number(L)),
    number(N1),
    not(number(L1)),
    N2 is N + N1,
    assert(node([L, L1],N2)),
    listing(node([L, L1],_)),

    !.

head_node([N, N2|List]):-
    not(one_element_list(List)),
    head_node(List),
   % List1=(B|_),
   % create_new_node(A,B,C),
    !.

head_node([N|List]):-
    one_element_list(List),
    create_new_node(N, List),

 %   create_new_node(A,List1,C),
 %   List=(A|C|List1),
    !.

create_vector([[L, _], [L1, _]]):-
     assert(vector([L, L1],l,0)),
     assert(vector([L, L1],l1,1)),
    !.

