let flatmap = (array, fn) => array->Belt.Array.map(fn)->Belt.Array.concatMany
