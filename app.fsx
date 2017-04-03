#r "bin/Liquid1.dll"
#r "packages/Suave/lib/net40/Suave.dll"

open Suave
open Suave.Http
open Suave.Filters
open Suave.Successful

open System
open System.Net

open Microsoft.Research.Liquid
open Util
open Operations
open Tests

module Script =
    // public method for Z rotating a particular qubit in the cluster state
    let rotate (q:Qubit) (angle:float) =
        HamiltonianGates.Rpauli angle Z [q]                

    // public method for measuring a qubit in the XY plane
    let rotatedMeasurement (q:Qubit) (angle:float) =
        HamiltonianGates.Rpauli -angle Z [q]
        H [q]
        //HamiltonianGates.Rpauli (Math.PI/2.0) Y [q]
        M [q]


    let test(pad:int, n:int, angle1, angle2, angle3)    =
        let mutable out1 = 0
        let mutable out2 = 0
        let mutable out3 = 0
        let mutable out4 = 0
        let mutable outout = 0
        let k = Ket(4)
        let mutable rangle1 = Math.PI / 2.
        let mutable rangle2 = Math.PI / 4.
        let mutable rangle3 = Math.PI / 2.

        if (angle1 % 4) = 0 then
            rangle1 <- 0.
        else
            if (angle1 % 4) = 1 then
                rangle1 <- Math.PI / 2.
            else
                if (angle1 % 4) = 2 then
                    rangle1 <- Math.PI
                else
                    rangle1 <- 3. * Math.PI / 2.

        if (angle2 % 4) = 0 then
            rangle2 <- Math.PI / 4.
        else
            if (angle2 % 4) = 1 then
                rangle2 <- -Math.PI / 4.
            else
                if (angle2 % 4) = 2 then
                    rangle2 <- Math.PI / 2.
                else
                    rangle2 <- -Math.PI / 2.
                    
        if (angle3 % 4) = 0 then
            rangle3 <- 0.
        else
            if (angle3 % 4) = 1 then
                rangle3 <- Math.PI / 2.
            else
                if (angle3 % 4) = 2 then
                    rangle3 <- Math.PI
                else
                    rangle3 <- 3. * Math.PI / 2.

        //let pad = 0
        for i in 1..n do
            let qs = k.Reset(4)
            // make Bell state and |+> states
            H >< qs
            H [qs.[1]]
            CNOT [qs.[3]; qs.[1]]
            X [qs.[1]]

            // entangle qubits
            CZ [qs.[0]; qs.[1]]
            CZ [qs.[1]; qs.[2]]

            // server measurements
            rotatedMeasurement qs.[0] rangle1
            rotatedMeasurement qs.[1] rangle2
            rotatedMeasurement qs.[2] rangle3

            //client measurement
            if ((qs.[0].Bit.v + pad) % 2) = 0 then                
                rotatedMeasurement qs.[3] 0.
            else
                rotatedMeasurement qs.[3] (Math.PI / 2.)

            // collect statistics
            out1 <- out1 + qs.[0].Bit.v
            out2 <- out2 + qs.[1].Bit.v
            out3 <- out3 + qs.[2].Bit.v
            out4 <- out4 + qs.[3].Bit.v

            let res = (pad + qs.[0].Bit.v + qs.[1].Bit.v + qs.[2].Bit.v + qs.[3].Bit.v) % 2
            outout <- outout + res
        
        show "Number of ones for each qubit: [%d, %d, %d, %d]" out1 out2 out3 out4
        show "Number of ones in output: %d" outout

        [outout; out1; out2; out3; out4]


let htmlf = (sprintf """
  <html>
  <head>
      <title>Plotly Chart</title>
      <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
  </head>
  <body><div id="61f0250d-dbf5-43eb-b33b-b2c348c3acdc" style="width: 700px; height: 500px;"></div>
        <script>
            var data = [{"type":"bar","x":["Client qubit","Server qubit 1","Server qubit 2","Server qubit 3","Server qubit 4"],"y":[%d,%d,%d,%d,%d],"name":"Input 0"},{"type":"bar","x":["Client qubit","Server qubit 1","Server qubit 2","Server qubit 3","Server qubit 4"],"y":[%d,%d,%d,%d,%d],"name":"Input 1"}];
            var layout = {"title":"Output statistics for %d trials","barmode":"group"};
            Plotly.newPlot('61f0250d-dbf5-43eb-b33b-b2c348c3acdc', data, layout);
        </script></body></html>
""")

let config = 
    let port = System.Environment.GetEnvironmentVariable("PORT")
    let ip127  = IPAddress.Parse("127.0.0.1")
    let ipZero = IPAddress.Parse("0.0.0.0")

    { defaultConfig with 
        logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Verbose
        bindings=[ (if port = null then HttpBinding.mk HTTP ip127 (uint16 8080)
                    else HttpBinding.mk HTTP ipZero (uint16 port)) ] }

let f n angle1 angle2 angle3 = async {
    return Script.test(0, n, angle1, angle2, angle3) @ Script.test(1, n, angle1, angle2, angle3)
}

let app =
  pathScan "/%d-%d-%d-%d" (fun (n : int, angle1 : int, angle2 : int, angle3 : int) -> fun x -> async{ let! r = (f n angle1 angle2 angle3) in return! OK (htmlf r.[0] r.[1] r.[2] r.[3] r.[4] r.[5] r.[6] r.[7] r.[8] r.[9] n) x })
startWebServer config app
