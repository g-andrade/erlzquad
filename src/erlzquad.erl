-module(erlzquad).
-compile(inline).
-author('Guilherme Andrade <erlzquad(at)gandrade(dot)net>').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_qtree/3]).   -ignore_xref([{new_qtree, 3}]).
-export([add_objects/3]). -ignore_xref([{add_objects, 3}]).
-export([query_area/5]).  -ignore_xref([{query_area, 5}]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(ZQUADRANT_BOTTOM_LEFT,  2#00).
-define(ZQUADRANT_BOTTOM_RIGHT, 2#01).
-define(ZQUADRANT_TOP_LEFT,     2#10).
-define(ZQUADRANT_TOP_RIGHT,    2#11).
-define(QUAD_NODE_CHILDREN_OFFSET, #quad_node.bottom_left).

-define(IS_POS_NUMBER(V), (is_number((V)) andalso (V) > 0)).
-define(IS_NONNEG_INTEGER(V), (is_integer((V)) andalso (V) >= 0)).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(quad_node, {
          bottom_left :: quad_node() | undefined,
          bottom_right :: quad_node() | undefined,
          top_left :: quad_node() | undefined,
          top_right :: quad_node() | undefined,
          bucket = [] :: [term()]
         }).
-type quad_node() :: #quad_node{}.

-record(quad_tree, {
          width :: number(),
          height :: number(),
          depth :: non_neg_integer(),
          root_node :: quad_node()
         }).
-type quad_tree() :: #quad_tree{}.
-export_type([quad_tree/0]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type object_box_fun() :: fun((Object :: term()) ->
                              {Left :: number(), Bottom :: number(),
                               Right :: number(), Top :: number()}).
-export_type([object_box_fun/0]).

-type zquadrant() :: 2#00 | 2#01 | 2#10 | 2#11.
-type single_rabbit_hole() :: [zquadrant()].
-type tree_rabbit_hole() :: [{Quadrant :: zquadrant(), SubRabbitHole :: [any()]}].
-type nonempty_tree_rabbit_hole() :: [{Quadrant :: zquadrant(), tree_rabbit_hole()}, ...].

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new_qtree(Width :: number(), Height :: number(), Depth :: non_neg_integer())
        -> quad_tree().
new_qtree(Width, Height, Depth) when ?IS_POS_NUMBER(Width),
                                     ?IS_POS_NUMBER(Height),
                                     ?IS_NONNEG_INTEGER(Depth) ->
    #quad_tree{width = Width,
               height = Height,
               root_node = new_node(Depth),
               depth = Depth}.

-spec add_objects(Objects :: [term()], GetBoxFun :: object_box_fun(), QTree :: quad_tree())
        -> quad_tree().
add_objects(Objects, GetBoxFun, QTree) ->
    #quad_tree{width = TreeWidth,
               height = TreeHeight,
               depth = Depth} = QTree,
    InvTreeWidth = 1 / TreeWidth,
    InvTreeHeight = 1 / TreeHeight,
    MaxX = TreeWidth - 1,
    MaxY = TreeHeight - 1,
    WithZees = [{object_zees(Object, Depth, MaxX, InvTreeWidth, MaxY, InvTreeHeight, GetBoxFun), Object}
                || Object <- Objects],
    Sorted = lists:keysort(1, WithZees),
    add_sorted_zeed_objects(Sorted, QTree).

-spec query_area(Left :: number(), Bottom :: number(), Right :: number(), Top :: number(),
                 QTree :: quad_tree())
        -> Objects :: [term()].
query_area(Left, Bottom, Right, Top, QTree) ->
    #quad_tree{width = TreeWidth,
               height = TreeHeight,
               root_node = RootNode,
               depth = Depth} = QTree,
    InvTreeWidth = 1 / TreeWidth,
    InvTreeHeight = 1 / TreeHeight,
    MaxX = TreeWidth - 1,
    MaxY = TreeHeight - 1,
    ZList = coords_to_zlist(cull(Left, 0, MaxX),
                            cull(Bottom, 0, MaxY),
                            cull(Right, 0, MaxX),
                            cull(Top, 0, MaxY),
                            InvTreeWidth, InvTreeHeight, Depth),
    BitZList = [z_to_bitz(Z, Depth) || Z <- ZList],
    RabbitHole = tree_rabbit_hole(BitZList),
    deep_query_area(RabbitHole, RootNode).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%
%% creating
%%
-spec new_node(Depth :: non_neg_integer()) -> quad_node().
new_node(0 = _Depth) ->
    #quad_node{};
new_node(Depth) ->
    #quad_node{top_left     = new_node(Depth - 1),
               top_right    = new_node(Depth - 1),
               bottom_left  = new_node(Depth - 1),
               bottom_right = new_node(Depth - 1)}.

%%
%% adding
%%
-spec object_zees(Object :: term(), Depth :: non_neg_integer(),
                  MaxX :: number(), InvTreeWidth :: float(),
                  MaxY :: number(), InvTreeHeight :: float(),
                  GetBoxFun :: object_box_fun())
        -> {LowZ :: non_neg_integer(), HighZ :: non_neg_integer()}.
object_zees(Object, Depth, MaxX, InvTreeWidth, MaxY, InvTreeHeight, GetBoxFun) ->
    {Left, Bottom, Right, Top} = GetBoxFun(Object),
    LowZ = coords_to_z(cull(Left, 0, MaxX),
                       cull(Bottom, 0, MaxY),
                       InvTreeWidth, InvTreeHeight, Depth),
    HighZ = coords_to_z(cull(Right, 0, MaxX),
                        cull(Top, 0, MaxY),
                        InvTreeWidth, InvTreeHeight, Depth),
    {LowZ, HighZ}.

-spec add_sorted_zeed_objects(ZeedObjects :: [{{LowZ :: non_neg_integer(), HighZ :: non_neg_integer()},
                                               Object :: term()}],
                              QTree :: quad_tree())
        -> quad_tree().
add_sorted_zeed_objects([], QTree) ->
    QTree;
add_sorted_zeed_objects([{Zees, Object} | NextObjects], QTree) ->
    add_sorted_zeed_objects_recur(NextObjects, Zees, [Object], QTree).

-spec add_sorted_zeed_objects_recur(ZeedObjects :: [{{LowZ :: non_neg_integer(), HighZ :: non_neg_integer()},
                                                     Object :: term()}],
                                    PrevZees :: {LowZ :: non_neg_integer(), HighZ :: non_neg_integer()},
                                    PrevGroup :: [Object :: term()],
                                    QTree :: quad_tree())
        -> quad_tree().
add_sorted_zeed_objects_recur([], PrevZees, PrevGroup, QTree) ->
    {PrevLowZ, PrevHighZ} = PrevZees,
    #quad_tree{root_node = RootNode,
               depth = Depth} = QTree,
    PrevLowBitZ = z_to_bitz(PrevLowZ, Depth),
    PrevHighBitZ = z_to_bitz(PrevHighZ, Depth),
    PrevRabbitHole = single_rabbit_hole(PrevLowBitZ, PrevHighBitZ),
    NewRootNode = deep_add_objects(PrevGroup, PrevRabbitHole, RootNode),
    QTree#quad_tree{ root_node = NewRootNode };
add_sorted_zeed_objects_recur([{Zees, Object} | NextObjects], Zees, GroupSoFar, QTree) ->
    add_sorted_zeed_objects_recur(NextObjects, Zees, [Object | GroupSoFar], QTree);
add_sorted_zeed_objects_recur([{Zees, Object} | NextObjects], PrevZees, PrevGroup, QTree) ->
    {PrevLowZ, PrevHighZ} = PrevZees,
    #quad_tree{root_node = RootNode,
               depth = Depth} = QTree,
    PrevLowBitZ = z_to_bitz(PrevLowZ, Depth),
    PrevHighBitZ = z_to_bitz(PrevHighZ, Depth),
    PrevRabbitHole = single_rabbit_hole(PrevLowBitZ, PrevHighBitZ),
    NewRootNode = deep_add_objects(PrevGroup, PrevRabbitHole, RootNode),
    NewQTree = QTree#quad_tree{ root_node = NewRootNode },
    add_sorted_zeed_objects_recur(NextObjects, Zees, [Object], NewQTree).

-spec deep_add_objects(Object :: [term()], RabbitHole :: single_rabbit_hole(), quad_node()) -> quad_node().
deep_add_objects(Objects, []=_RabbitHole, #quad_node{ bucket = PrevBucket }=QNode) ->
    QNode#quad_node{ bucket = Objects ++ PrevBucket };
deep_add_objects(Objects, [Quadrant | NextQuadrants], QNode) ->
    ChildPos = Quadrant + ?QUAD_NODE_CHILDREN_OFFSET,
    Child = element(ChildPos, QNode),
    NewChild = deep_add_objects(Objects, NextQuadrants, Child),
    setelement(ChildPos, QNode, NewChild).

%%
%% querying
%%
-spec deep_query_area(RabbitHole :: tree_rabbit_hole(),
                      quad_node()) -> Objects :: [term()].
deep_query_area([] = _RabbitHole, #quad_node{ bucket = Bucket, top_left = undefined }) ->
    %
    % Leaf node.
    %
    Bucket;

deep_query_area([_|_] = RabbitHole,  #quad_node{ bucket = Bucket } = QNode) ->
    %
    % Intermediate node.
    %
    lists:foldl(
      fun ({Quadrant, SubRabbitHole}, ObjectsAcc) ->
              ChildPos = Quadrant + ?QUAD_NODE_CHILDREN_OFFSET,
              Child = element(ChildPos, QNode),
              deep_query_area(SubRabbitHole, Child) ++ ObjectsAcc
      end,
      Bucket,
      RabbitHole).

%%
%% Z-order curve!
%%
-spec coords_to_z(X :: number(), Y :: number(), InvMaxX :: float(), MaxY :: float(),
                  Depth :: non_neg_integer())
        -> non_neg_integer().
coords_to_z(X, Y, InvMaxX, InvMaxY, Depth) ->
    MaxCoordZ = (1 bsl Depth),
    ZX = trunc(X * InvMaxX * MaxCoordZ),
    ZY = trunc(Y * InvMaxY * MaxCoordZ),
    interleave(ZX, ZY, Depth).

coords_to_zlist(Left, Bottom, Right, Top, InvMaxX, InvMaxY, Depth) ->
    MaxCoordZ = (1 bsl Depth),
    LeftZ = trunc(Left * InvMaxX * MaxCoordZ),
    BottomZ = trunc(Bottom * InvMaxY * MaxCoordZ),
    RightZ = trunc(Right * InvMaxX * MaxCoordZ),
    TopZ = trunc(Top * InvMaxY * MaxCoordZ),
    lists:sort([interleave(ZX, ZY, Depth)
                || ZX <- lists:seq(LeftZ, RightZ),
                   ZY <- lists:seq(BottomZ, TopZ)]).


-spec interleave(ZX :: non_neg_integer(), ZY :: non_neg_integer(), Depth :: non_neg_integer())
        -> non_neg_integer().
interleave(ZX, ZY, 8=_Depth) ->
    % Woah. Is this the Matrix?
    (((ZY band 2#00000001) bsl 1) bor ((ZX band 2#00000001))) bor
    (((ZY band 2#00000010) bsl 2) bor ((ZX band 2#00000010) bsl 1)) bor
    (((ZY band 2#00000100) bsl 3) bor ((ZX band 2#00000100) bsl 2)) bor
    (((ZY band 2#00001000) bsl 4) bor ((ZX band 2#00001000) bsl 3)) bor
    (((ZY band 2#00010000) bsl 5) bor ((ZX band 2#00010000) bsl 4)) bor
    (((ZY band 2#00100000) bsl 6) bor ((ZX band 2#00100000) bsl 5)) bor
    (((ZY band 2#01000000) bsl 7) bor ((ZX band 2#01000000) bsl 6)) bor
    (((ZY band 2#10000000) bsl 8) bor ((ZX band 2#10000000) bsl 7));
interleave(ZX, ZY, 7=_Depth) ->
    (((ZY band 2#0000001) bsl 1) bor ((ZX band 2#0000001))) bor
    (((ZY band 2#0000010) bsl 2) bor ((ZX band 2#0000010) bsl 1)) bor
    (((ZY band 2#0000100) bsl 3) bor ((ZX band 2#0000100) bsl 2)) bor
    (((ZY band 2#0001000) bsl 4) bor ((ZX band 2#0001000) bsl 3)) bor
    (((ZY band 2#0010000) bsl 5) bor ((ZX band 2#0010000) bsl 4)) bor
    (((ZY band 2#0100000) bsl 6) bor ((ZX band 2#0100000) bsl 5)) bor
    (((ZY band 2#1000000) bsl 7) bor ((ZX band 2#1000000) bsl 6));
interleave(ZX, ZY, 6=_Depth) ->
    (((ZY band 2#000001) bsl 1) bor ((ZX band 2#000001))) bor
    (((ZY band 2#000010) bsl 2) bor ((ZX band 2#000010) bsl 1)) bor
    (((ZY band 2#000100) bsl 3) bor ((ZX band 2#000100) bsl 2)) bor
    (((ZY band 2#001000) bsl 4) bor ((ZX band 2#001000) bsl 3)) bor
    (((ZY band 2#010000) bsl 5) bor ((ZX band 2#010000) bsl 4)) bor
    (((ZY band 2#100000) bsl 6) bor ((ZX band 2#100000) bsl 5));
interleave(ZX, ZY, 5=_Depth) ->
    (((ZY band 2#00001) bsl 1) bor ((ZX band 2#00001))) bor
    (((ZY band 2#00010) bsl 2) bor ((ZX band 2#00010) bsl 1)) bor
    (((ZY band 2#00100) bsl 3) bor ((ZX band 2#00100) bsl 2)) bor
    (((ZY band 2#01000) bsl 4) bor ((ZX band 2#01000) bsl 3)) bor
    (((ZY band 2#10000) bsl 5) bor ((ZX band 2#10000) bsl 4));
interleave(ZX, ZY, 4=_Depth) ->
    (((ZY band 2#0001) bsl 1) bor ((ZX band 2#0001))) bor
    (((ZY band 2#0010) bsl 2) bor ((ZX band 2#0010) bsl 1)) bor
    (((ZY band 2#0100) bsl 3) bor ((ZX band 2#0100) bsl 2)) bor
    (((ZY band 2#1000) bsl 4) bor ((ZX band 2#1000) bsl 3));
interleave(ZX, ZY, 3=_Depth) ->
    (((ZY band 2#001) bsl 1) bor ((ZX band 2#001))) bor
    (((ZY band 2#010) bsl 2) bor ((ZX band 2#010) bsl 1)) bor
    (((ZY band 2#100) bsl 3) bor ((ZX band 2#100) bsl 2));
interleave(ZX, ZY, 2=_Depth) ->
    (((ZY band 2#01) bsl 1) bor ((ZX band 2#01))) bor
    (((ZY band 2#10) bsl 2) bor ((ZX band 2#10) bsl 1));
interleave(ZX, ZY, 1=_Depth) ->
    (((ZY band 2#1) bsl 1) bor ((ZX band 2#1)));
interleave(_ZX, _ZY, 0=_Depth) ->
    0;
interleave(ZX, ZY, Depth) ->
    ((((ZY band 1) bsl 1) bor (ZX band 1)) bsl (2 * (Depth - 1))) bor
    interleave(ZX bsr 1, ZY bsr 1, Depth - 1).

-spec z_to_bitz(Z :: non_neg_integer(), Depth :: non_neg_integer()) -> bitstring().
z_to_bitz(Z, Depth) ->
    % Stuff those nasty bits in
    BitCount = Depth bsl 1,
    <<Z:BitCount/unit:1>>.

-spec single_rabbit_hole(Z1 :: bitstring(), Z2 :: bitstring()) -> single_rabbit_hole().
single_rabbit_hole(<<ZQuadrant:2/unit:1, SubZ1/bitstring>>,
                   <<ZQuadrant:2/unit:1, SubZ2/bitstring>>)
->
    % Still agreeing; continue.
    [ZQuadrant | single_rabbit_hole(SubZ1, SubZ2)];
single_rabbit_hole(_Z1, _Z2) ->
    % The end of the line; encompass everything from this point on.
    [].

-spec tree_rabbit_hole(ZList :: [bitstring()]) -> tree_rabbit_hole().
tree_rabbit_hole([<<Quadrant:2/unit:1, SubQuadrants/bitstring>> | NextZees]) ->
    tree_rabbit_hole_recur(NextZees, Quadrant, [SubQuadrants]);
tree_rabbit_hole(_) ->
    [].

-spec tree_rabbit_hole_recur(ZList :: [bitstring()], Quadrant :: zquadrant(),
                             GroupSoFar :: [bitstring(), ...]) -> nonempty_tree_rabbit_hole().
tree_rabbit_hole_recur([<<Quadrant:2/unit:1, SubQuadrants/bitstring>> | NextZees],
                       Quadrant, GroupSoFar) ->
    tree_rabbit_hole_recur(NextZees, Quadrant, [SubQuadrants | GroupSoFar]);
tree_rabbit_hole_recur([<<Quadrant:2/unit:1, SubQuadrants/bitstring>> | NextZees],
                   PrevQuadrant, PrevGroup) ->
    Acc = [{PrevQuadrant, tree_rabbit_hole(PrevGroup)}],
    Acc ++ tree_rabbit_hole_recur(NextZees, Quadrant, [SubQuadrants]);
tree_rabbit_hole_recur([], PrevQuadrant, PrevGroup) ->
    [{PrevQuadrant, tree_rabbit_hole(PrevGroup)}].

-spec cull(Value :: number(), Min :: number(), Max :: number()) -> number().
cull(Value, Min, Max) ->
    max(Min, min(Max, Value)).
