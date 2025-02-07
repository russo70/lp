:- dynamic hucodec_decode/3.
:- dynamic hucodec_encode/3.
:- dynamic hucodec_encode_file/3.
:- dynamic hucodec_generate_huffman_tree/2.
:- dynamic hucodec_generate_symbol_bits_table/2.
:- dynamic hucode_print_huffman_tree/1.

void_list([]):-!.

one_element_list(List):-
    List=(_|L),
    void_list(L),
    !.



%hucodec_generate_huffman_tree

hucodec_generate_huffman_tree(List, Tree):-
    not(void_list(List)),
    not(one_element_list(List)),
    Tree=0,
    !.


create_new_node(A, B, C):-
    A=(L, N),
    number(N),
    not(number(L)),
    B=(L1,N1);
    number(N1),
    not(number(L1)),
    C=((L,L1),N + N1),
    writeln(C),nl,
    !.

head_node(List):-
    List=(),
    !.
