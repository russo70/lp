:- dynamic node/2.
:- dynamic vector/3.
:- dynamic symbols_n_weights/1.
:- dynamic message/1.
:- dynamic hucodec_generate_symbol_bits_table/2.
:- dynamic hucode_print_huffman_tree/1.

void_list([]):-!.

one_element_list([_]):-!.

is_only_numbers(String) :-
  string_codes(String, Codes),
    forall(member(Code, Codes),
         between(48, 57, Code)).

cancella_tutti_nodi :-
    retractall(node(_, _)).

cancella_tutti_vettori :-
    retractall(vector(_, _,_)).

cancella_symbols_n_weights:- retractall(symbols_n_weights(_)).

cancella_messaggio:- retractall(message(_)).


delete_all:-
    cancella_tutti_nodi,
    cancella_tutti_vettori,
    cancella_symbols_n_weights,
    cancella_messaggio,
    !.

%hucodec_encode/3

hucodec_encode(Message, Tree, Bit):-
    convert_to_list(Message, List),
    encode(List, Tree, Bit),
    !.

encode([M|List], Tree, Bit):-
     not(void_list(List)),
     get_bit(M,Tree, Bitx),
     encode(List, Tree, Bits),
      string_concat(Bitx, Bits, Bit),

    !.

encode([M|List], Tree, Bit):-
    void_list(List),
    get_bit(M,Tree, Bit),
    !.


get_bit(M, [Tree|_], Bit):-
    not(one_element_list(Tree)),
    member(M, Tree),
    findall(N, vector(Tree, N, 1), [List|_]),
    is_list(List),
    member(M,List),
    Bitx = "1",
    get_bit(M, [List,_],Bits),
    string_concat(Bitx, Bits, Bit),
    !.

get_bit(M, [Tree|_], Bit):-
    not(one_element_list(Tree)),
    member(M, Tree),
    findall(N, vector(Tree, N, 1), [List|_]),
    not(is_list(List)),
    List1 = [List],
    member(M,List1),
    Bit = "1",
    !.

get_bit(M, [Tree|_], Bit):-
    not(one_element_list(Tree)),
    member(M, Tree),
    findall(N, vector(Tree, N, 0), [List|_]),
    is_list(List),
    member(M,List),
    Bit = "0",
    get_bit(M, [List,_],Bits),
     string_concat(Bit, Bits, Bit),
    !.

get_bit(M, [Tree|_], Bit):-
    not(one_element_list(Tree)),
    member(M, Tree),
    findall(N, vector(Tree, N, 0), [List|_]),
    not(is_list(List)),
    List1 = [List],
    member(M,List1),
    Bit = "0",
    !.


convert_to_list(M, List):-
    atom_chars(M, List),
    !.



%hucodec_encode_file/3

hucodec_encode_file(Filename,Tree,Bits):-
  leggi_file(Filename,String),
  hucodec_encode(String,Tree,Bits),
  !.

leggi_file(NomeFile, Contenuto) :-
    open(NomeFile, read, Stream),
    read_string(Stream, _, Contenuto),
    close(Stream),
    !.

%hucodec_decode/3

hucodec_decode(Bits, [Tree|_], Message):-
    is_only_numbers(Bits),
    convert_to_list(Bits, List),
    get_string(List,Tree,Message),

    !.

get_string(Bits,Tree,Message):-
    not(void_list(Bits)),
    get_letter(Bits,Tree,Bit,M),
    get_string(Bit,Tree,M1),
    atom_concat(M, M1, Message),
  !.

get_string(Bits,_,M):-
  void_list(Bits),
  M = "",
  !.

get_letter([N|Bits],Tree,Bit,M):-
  atom_number(N,N1),
  findall(L, vector(Tree, L, N1), [List2|_]),
  not(one_element_list(List2)),
  get_letter(Bits,List2,Bit,M),
  !.

get_letter([N|Bits],Tree,Bits,M):-
   atom_number(N,N1),
   findall(List, vector(Tree, List, N1), Ntree),
   one_element_list(Ntree),
   Ntree =[M|_],

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
    assert(node(L,N)),
    assert(node(L1,N1)),
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
     assert(node(L1,N1)),
    assert(node(Result,N2)),
    create_vector([[L, N], [L1, N1]]),
    !.

create_new_node([[L, N], [L1, N1]]):-
   not(is_list(L)),
   is_list(L1),
    append([L], L1, Result),
    N2 is N + N1,
     assert(node(L,N)),
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
     assert(vector([L, L1],L,0)),
     assert(vector([L, L1],L1,1)),
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

hucodec_print_huffman_tree([Tree|_]):-
    not(one_element_list(Tree)),
    % writeln(Tree),
    findall(N, vector(Tree, N, 0), [List|_]),
    writeln( vector(Tree, List, 0)),
    hucodec_print_huffman_tree([List]),
    findall(N2, vector(Tree, N2, 1), [List1|_]),
     writeln( vector(Tree, List1, 1)),
    hucodec_print_huffman_tree([List1]),
    !.

hucodec_print_huffman_tree(Tree):-
        one_element_list(Tree),
        Tree =[Node],
         findall(N, node(Node, N), [List|_]),
          writeln( node(Node, List)),
    !.
