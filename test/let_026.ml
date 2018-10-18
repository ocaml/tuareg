let a = {
  b = (
    let z = f u in
    z + z;
  );
  c = (let a = b in {
         z = z;
         y = h;
      });
}
