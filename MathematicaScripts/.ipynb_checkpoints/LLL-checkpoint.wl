(* ::Package:: *)

(* Function to evaluate a numerical integral with precision options *)
EvaluateIntegral[integrand_, {var_, lower_, upper_}, workingPrecision_, precisionGoal_] := 
    NIntegrate[integrand, {var, lower, upper}, 
               MaxRecursion -> 100, 
               WorkingPrecision -> workingPrecision, 
               PrecisionGoal -> precisionGoal]
               
LatRed[fitlist_List,z_,digits_]:=Module[{},
v2=Append[Prepend[Round[10^digits fitlist],Round[10^digits z]],10^digits]; (* Makes into list of integers *)
FillArray[q_,y_]:=Which[q!= y&&y!= Length[v2],0,q==y,1,True,v2[[q]]]; 
a2=Array[FillArray,{Length[v2]-1,Length[v2]}]; (* makes a lattice out of integers { 1,0,...,0,n1}, {0,1,0...0,n2},...} *)
b2=LatticeReduce[a2];
bu1 = Prepend[Append[fitlist,0],u];
u/.Solve[b2[[1]] . bu1==0,u][[1]]//Expand
]

ArrayLatRed[fitarray_,yarray_,digits_]:=Module[{dims,  res, norm},dims=Dimensions[fitarray];
basisin=ArrayFlatten[{{Prepend[Round[10^digits fitarray]//Transpose,Round[10^digits yarray] ],IdentityMatrix[dims[[2]]+1]}}];
basered=LatticeReduce[basisin];
res =basered[[1]][[dims[[1]]+2;;-1]];
norm =-basered[[1]][[dims[[1]]+1]];
res/norm
]



