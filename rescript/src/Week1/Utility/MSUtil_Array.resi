let flatMap: (array<'a>, 'a => array<'b>) => array<'b>
let takeFirstWithDefault: (array<'a>, 'a) => 'a
let everyO : (array<option<'a>>)=>option<array<'a>>
let take : (array<'a>, array<int>) => option<array<'a>>
let has:(array<'a>,'a)=>bool