module AreTheySame = 
    let comp a b = 
     let newa = a |> List.map (fun x -> x*x) |> List.sort
     let newb = b |> List.sort
     newa = newb
