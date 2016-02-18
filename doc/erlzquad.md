

# Module erlzquad #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-object_box_fun">object_box_fun()</a> ###



<pre><code>
object_box_fun() = fun((Object::term()) -&gt; {Left::number(), Bottom::number(), Right::number(), Top::number()})
</code></pre>





### <a name="type-quad_tree">quad_tree()</a> ###



<pre><code>
quad_tree() = #quad_tree{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_objects-3">add_objects/3</a></td><td></td></tr><tr><td valign="top"><a href="#new_qtree-3">new_qtree/3</a></td><td></td></tr><tr><td valign="top"><a href="#query_area-5">query_area/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_objects-3"></a>

### add_objects/3 ###


<pre><code>
add_objects(Objects::[term()], GetBoxFun::<a href="#type-object_box_fun">object_box_fun()</a>, QTree::<a href="#type-quad_tree">quad_tree()</a>) -&gt; <a href="#type-quad_tree">quad_tree()</a>
</code></pre>

<br></br>



<a name="new_qtree-3"></a>

### new_qtree/3 ###


<pre><code>
new_qtree(Width::number(), Height::number(), Depth::non_neg_integer()) -&gt; <a href="#type-quad_tree">quad_tree()</a>
</code></pre>

<br></br>



<a name="query_area-5"></a>

### query_area/5 ###


<pre><code>
query_area(Left::number(), Bottom::number(), Right::number(), Top::number(), QTree::<a href="#type-quad_tree">quad_tree()</a>) -&gt; Objects::[term()]
</code></pre>

<br></br>



