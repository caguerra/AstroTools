(* ::Package:: *)

(* ::Title:: *)
(*Utilities*)


(* ::Subsection:: *)
(*LoadPackage*)


BeginPackage["AstroTools`Utilities`"]


(* Memory *)
TimeAndMem::usage;
DeleteOuts::usage;
PrintArrayInfo::usage;

(* Convertions *)
StringToNumbers::usage;

(* Calculations *)
TotalPotentialEnergy::usage = "TotalPotentialEnergy[pos_, vel_]";
TotalKineticEnergy::usage;

(* Files *)
StarlabSnapToNBody;

(* Plots and simulations *)
ScaledListPlot::usage;
ClusterPlot::usage = "ClusterPlot[{pos, cm, mcm}, t] plots the starts at time t"


$TimeScale


Begin["`Private`"]


(* ::Subsection:: *)
(*Memory*)


memToString[mem_] := Module[{memString},
		memString = 
			Which[
				0 <= mem < 1024, ToString[mem] <> "B",
				1024 <= mem < 1024*1024, ToString[mem/1024.] <> "KB",
				1024*1024 <= mem < 1024*1024*1024, ToString[mem/1024./1024.] <> "MB", 
				1024*1024*1024 <= mem < Infinity, ToString[mem/1024./1024./1024.] <> "GB"
			];
		memString
	];

TimeAndMem[] := DateString[{"Hour",":","Minute",":","Second"}] <> " mem:" <> memToString[MemoryInUse[]];

SetAttributes[PrintArrayInfo, HoldAll];

PrintArrayInfo[a_] := Print["Length: ", Length[a], " ", "MEM: ", memToString[ByteCount[a]] ];

DeleteOuts[mem_] := 
	Module[
		{outs},

		Print["session mem: ", memToString[MemoryInUse[]] ];

		outs = Cases[Table[{k, ByteCount[Out[k]]/1024./1024.}, {k,346}], {_,m_} /; m > mem][[All,1]]; 

		Unprotect[Out];
		Do[Out[out]=., {out,outs}];
		Protect[Out];

		Print["session mem: ", memToString[MemoryInUse[]] ];	
	]


(* ::Subsection:: *)
(*Conversions*)


StringToNumbers[string_] := 
	Module[{strm, numbers},
		strm = StringToStream[string];
		numbers = ReadList[strm, Number]; (* works fine for V > 9*)
		Close[strm];
		If[Length[numbers]===1, numbers[[1]], numbers]
	]

(*
StringToNumbers[string_String] := Internal`StringToDouble[string]
StringToNumbers[list_List] := Internal`StringToDouble[#]& /@ list
*)


(* ::Subsection:: *)
(*Calculations*)


TotalKineticEnergy[mass_List, vel_List] := 0.5 * MapThread[#1.(Norm /@ #2)^2&, {mass, vel}]


TotalPotentialEnergy = 
	Compile[{{mass,_Real,2}, {pos,_Real,3}},
		Module[{sum, tmax, nmax},
			tmax = Length[mass];
			nmax = Length /@ mass;
			Table[
				sum = 0.;
				Do[ 
					sum = sum + mass[[t,i]] mass[[t,j]]/
						(\[Sqrt]((pos[[t,i,1]]-pos[[t,j,1]])^2+(pos[[t,i,2]]-pos[[t,j,2]])^2+(pos[[t,i,3]]-pos[[t,j,3]])^2)),
				{i, nmax[[t]]}, {j, i-1}
				];
				-sum,
			{t, 1, tmax}]
		],
		CompilationTarget -> "C"
	]

(*
TotalPotentialEnergy := 
	Compile[{{mass, _Real, 2}, {pos, _Real, 3}},
		Module[{sum},
			Table[
				sum=0.;
				Do[sum = sum + If[i==j, 0., (mass[[t,i]] mass[[t,j]])/Sqrt[(pos[[t,i,1]]-pos[[t,j,1]])^2+(pos[[t,i,2]]-pos[[t,j,2]])^2+(pos[[t,i,3]]-pos[[t,j,3]])^2]],{i, 1000},{j, 1000}];
				sum, {t, 1000}
			]
		]
	]
*)


(* ::Subsection:: *)
(*Plots*)


Clear[ScaledListPlot];

Options[ScaledListPlot] = Options[ListPlot];

ScaledListPlot[list:{{{_?NumberQ, _?NumberQ}..}..}, {scalex_, scaley_}, opts:OptionsPattern[]] := 
	Module[{newlist},
		newlist = Transpose[{scalex, scaley} Transpose[#]] & /@ list;
		ListLinePlot[newlist, 
			opts, 
			Frame -> True, 
			GridLines -> Automatic]
	];

ScaledListPlot[list:{{{_?NumberQ, _?NumberQ}..}..}, opts:OptionsPattern[]] := ScaledListPlot[list, {1, 1}, opts];

ScaledListPlot[list:{{{_?NumberQ, _?NumberQ}..}..}, scale_, opts:OptionsPattern[]] := ScaledListPlot[list, {$TimeScale, scale}, opts];

ScaledListPlot[list:{{_?NumberQ, _?NumberQ}..}, {scalex_, scaley_}, opts:OptionsPattern[]] := 
	Module[{newlist},
		newlist = Transpose[{scalex, scaley} Transpose[list]];
		ListLinePlot[newlist, 
			opts, 
			Frame -> True, 
			GridLines -> Automatic]
	];

ScaledListPlot[list:{{_?NumberQ, _?NumberQ}..}, opts:OptionsPattern[]] := ScaledListPlot[list, {1, 1}, opts];

ScaledListPlot[list:{{_?NumberQ, _?NumberQ}..}, scale_, opts:OptionsPattern[]] := ScaledListPlot[list, {$TimeScale, scale}, opts];

ScaledListPlot[list:{_?NumberQ..}, {scalet_, scaley_}, opts:OptionsPattern[]] :=
	Module[{time, len},
		len = Length[list]; 
		time = scalet Range[0, len - 1];
		ListLinePlot[Transpose[{time, scaley list}],
			opts, 
			Frame -> True, 
			GridLines -> Automatic]
];

ScaledListPlot[list:{_?NumberQ..}, opts:OptionsPattern[]] := ScaledListPlot[list, {1, 1}, opts];

ScaledListPlot[list:{_?NumberQ..}, scale_, opts:OptionsPattern[]] := ScaledListPlot[list, {$TimeScale, scale}, opts];

ScaledListPlot[list:{{_?NumberQ ..}..}, {scalet_, scaley_}, opts:OptionsPattern[]] :=
	Module[{time, len},
		len = Length[First[list]]; 
		time = scalet Range[0, len - 1];
		ListLinePlot[ Transpose[{time, scaley #}]& /@ list,
			opts, 
			Frame -> True, 
			GridLines -> Automatic]
];

ScaledListPlot[list:{{_?NumberQ..}..}, opts:OptionsPattern[]] := ScaledListPlot[list, {1, 1}, opts];

ScaledListPlot[list:{{_?NumberQ..}..}, scale_, opts:OptionsPattern[]] := ScaledListPlot[list, {$TimeScale, scale}, opts];


Clear[ClusterPlot]

Options[ClusterPlot] = Join[{"Proyection" -> {1,2}, "Scale" -> {1,1}, "Window" -> 50 }, Options[Plot]];

ClusterPlot[{pos_, cm_, mcm_}, time_, opts:OptionsPattern[]] :=
	Module[
		{window, proy, plot1, plot2, scale},

		window = OptionValue["Window"];
		proy = OptionValue["Proyection"];
		scale = OptionValue["Scale"];
		
		plot1 = ScaledListPlot[pos[[time, All, proy]], scale, 
					Sequence@@FilterRules[{opts}, Plot],
					Joined -> False, GridLines -> None, PlotStyle -> PointSize[0.008], 
					Axes -> False, PlotRange -> {{-window, window}, {-window, window}}];
		
		plot2 = Graphics[{AbsolutePointSize[5], 
					Red, Point[ cm[[time, proy]]], 
					Green, Point[ mcm[[time, proy]]]}];
		
		Show[plot1, plot2]
	]



(* ::Subsection:: *)
(*Files*)


Options[StarlabSnapToNBody] = 
			{"InputDirectory" -> Automatic,
			 "OutputDirectory" -> Automatic};

StarlabSnapToNBody[file_, opts:OptionsPattern[]] := 
	Module[{data, inDir, outDir},
		inDir = OptionValue["InputDirectory"];
		outDir = OptionValue["OutputDirectory"];
		If[ inDir =!= Automatic, SetDirectory[inDir]];
		data = Import[file];
		If[ inDir =!= Automatic, ResetDirectory[]];
		data = DeleteCases[data, {";"|";;", ___}|{}];
		data = data[[All,2;;-1]];
		If[ outDir =!= Automatic,
			If[DirectoryQ[outDir] === False,
				CreateDirectory[outDir]; 
				SetDirectory[outDir]] 
		];
		Export["fort.10", data, "Table"]
		If[ outDir =!= Automatic, ResetDirectory[]];
	]


(* ::Subsection:: *)
(*Simulations*)


Clear@convertTo3D;
convertTo3D[stars_,rangos_,tam_:{1,1,1}]:=
	Module[{pixels,corner,size,pos,maxcount},
		corner = Min/@rangos;
		size = Round[Abs[Subtract@@@rangos]/tam]+{1,1,1};
		pos = 
			DeleteCases[
				Cases[Tally[Round[Table[(s-corner)/tam+{1,1,1},{s,stars}]]], {{__?Positive},_}],
					{{x_,y_,z_}/;x>size[[1]]||y>size[[2]]||z>size[[3]],_}];
		maxcount = Max[pos[[All,-1]]];
		pos[[All,-1]] = .5+pos[[All,-1]]/maxcount/2;
		pixels = SparseArray[Rule@@@pos,size];
		Image3D[pixels]
];


(* ::Subsection:: *)
(*End*)


End[ ]


EndPackage[ ]
