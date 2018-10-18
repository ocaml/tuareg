module MB = struct
  type b =
    int
  [@@b]

  type b2 = int
  [@@b2]

  module MC = struct
    let v = 10
    [@@v]

    let rec v2 = 20
    and v3 = 30
    [@@v4]
  end
                [@@m] (* TODO: fix *)
end
