:- dynamic hucodec_decode/3.
:- dynamic hucodec_encode/3.
:- dynamic hucodec_encode_file/3.
:- dynamic hucodec_generate_huffman_tree/2.
:- dynamic hucodec_generate_symbol_bits_table/2.
:- dynamic hucode_print_huffman_tree/1.

void_list([]):-!.

one_element_list([_]):-!.



%hucodec_generate_huffman_tree

hucodec_generate_huffman_tree(List, Tree):-
    is_list(List),
    not(void_list(List)),
    not(one_element_list(List)),
    head_node(List,C):
    Tree=C,
    writeln(Tree),
    !.


create_new_node(A, B, C):-
    A=(L,N),
    number(N),
    not(number(L)),
    B=(L1,N1);
    number(N1),
    not(number(L1)),
    C=((L,L1),N + N1),
    writeln(C),nl,
    !.

head_node(List,C):-
    List1=(A|List),
    not(one_element_list(List1)),
    head_node(List1,C),
    List1=(B|_),
    create_new_node(A,B,C),
    !.

head_node(List,C):-
    List=(A|List1),
    one_element_list(List1),
    create_new_node(A,List1,C),
    List=(A|C|List1),
    !.

