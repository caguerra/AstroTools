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
BinarySerializeSave::usage;
BinaryDeserializeGet::usage;

(* Convertions *)
StringToNumbers::usage;

(* Calculations *)
TotalPotentialEnergy::usage = "TotalPotentialEnergy[pos_, vel_]";
TotalKineticEnergy::usage = "TotalKineticEnergy[mass_, pos_]";
VirialRadius::usage = "VirialRadius[mass_, pos_]"
EnergyFromPairs::usage = ""
FindBinaries::usage = ""
InstantaneousMultiples::usage = ""
PermanentBinaries::usage = ""
DensityCenter::usage = "DensityCenter[id, mass, pos] computes the density center from snapshot with (id, mass, pos) at given instant. It uses 12 neighboorhoods"
NeighborhoodDensity::usage = "DensityCenter[id, mass, pos] computes the density from snapshot with (id, mass, pos) at given instant. It uses 12 neighboorhoods"
LagrangianRadii::usage
LagrangianRadiiAndMasses::usage

(* Fileg *)
StarlabSnapToNBody;

(* Plots and simulations *)
ScaledListPlot::usage = "ScaledListPlot[list_] ";
ClusterPlot::usage = "ClusterPlot[{pos, cm, mcm}, t] plots the star cluster at time t"
ClusterPlot3D::usage = "ClusterPlot3D[{pos, cm, mcm}, t] plots the stars cluster at time t"
MinimalistHistogram::usage = "MinimalistHistogram[data, spec] make a histogram from data"
ZoomGraphics::usage = "Zoom in two dimensional graphcis. Useful for cluster plots"

(* Equivalence relations *)
$MetersToParsec = 3.240779289469756`*^-17;
$SecondToMegaYear = 3.1709791983764584`*^-14;
$JouleToErgs = 10^7;
$ParsecToAU = 206264.8062454803`;

(* Astrophysical paramenters *)
$GravitationalConstant = 6.67 * 10^-11;
$SolarMass = 1.988 * 10^30;

(* Scaling factors *)
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

Clear[DeleteOuts];

DeleteOuts[memInMB_:500] :=
	Module[
		{outs, boole},

		Print["session mem: ", memToString[MemoryInUse[]] ];

		outs = Cases[Table[{k, ByteCount[Out[k]]/1024./1024.}, {k, $Line}], {_,m_} /; m > memInMB];

		If[outs == {},
			Print["No Out[] greater than ", ToString[memInMB], "MB"]; Return[],
			Print["Out with mem greater than ", ToString[memInMB], "MP: ", Rule@@@outs] ];

		boole = ChoiceDialog["Do you want to delete Outs?"];
 		If[ boole === True,
 			Print["Deleting outputs ..."];
			Unprotect[Out];
			Do[Out[out]=., {out, outs[[All,1]]}];
			Protect[Out],
			Print["Canceled"]
		];

		Print["session mem: ", memToString[MemoryInUse[]] ];
	];

BinarySerializeSave[expr_, fileName_] :=
	Module[{bin, file, ow},
    	bin = BinarySerialize[expr];
    	file = CreateFile[fileName];
    	ow = OpenWrite[file, BinaryFormat -> True];
    	BinaryWrite[ow, bin];
    	Close[ow];
    ];


BinaryDeserializeGet[fileName_] :=
	BinaryDeserialize[ByteArray[BinaryReadList[fileName, "Byte"]]]


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

Clear[NeighborhoodDensity]
NeighborhoodDensity[id_, mass_, pos_] :=
	Module[
		{massRules, posRules, nearestFunction, density},
		massRules = Dispatch@Thread[id -> mass];
		posRules = Dispatch@Thread[id -> pos];
		nearestFunction = Nearest[MapThread[Rule,{pos, id}]];
		density =
			With[{neighborhood = Rest[nearestFunction[#,12]]},
				(3./(4.*Pi)) Total[neighborhood /. massRules] / EuclideanDistance[#, Last[neighborhood] /. posRules]^3]& /@ pos
	]


Clear[DensityCenter];
DensityCenter[id_, mass_, pos_]:=
	Module[
		{massRules, posRules, nearestFunction, density},
		massRules = Dispatch@Thread[id -> mass];
		posRules = Dispatch@Thread[id -> pos];
		nearestFunction = Nearest[MapThread[Rule,{pos, id}]];
		density =
			With[{neighborhood = Rest[nearestFunction[#,12]]},
				Total[neighborhood /. massRules]/EuclideanDistance[#, Last[neighborhood] /. posRules]^3]& /@ pos;
		Total[density*pos] / Total[density]
	]

Clear[LagrangianRadii];
LagrangianRadii[mass_, position_, densityCenter_, radii_:Range[0, 1.0, 0.1]]:=
	Module[{v1, v2},
		(* Sort masses by distance to density center *)
		v1 = SortBy[Transpose[{mass, Norm[# - densityCenter]& /@ position}], Last];
		(* Accumulate masses *)
		v2 = Transpose[{Accumulate[v1[[All,1]]], v1[[All,2]]}];
		If[#!={}, #[[-1,-1]], Missing[]]& /@ (Cases[v2, {x_, d_} /; #1 < x <= #2]& @@@ Partition[radii, 2, 1])
	]

Clear[LagrangianRadiiAndMasses];
LagrangianRadiiAndMasses[mass_, position_, densityCenter_, radii_:Range[0, 1.0, 0.1]]:=
	Module[{v1, v2},
		v1 = SortBy[Transpose[{mass, Norm[# - densityCenter]& /@ position}], Last];
		v2 = Transpose[{Accumulate[v1[[All,1]]], v1[[All,2]]}];
		If[#!={}, #[[-1]], {Missing[], Missing[]}]& /@ (Cases[v2, {x_, d_} /; #1 < x <= #2]& @@@ Partition[radii, 2, 1])
	]


Clear[TotalKineticEnergy]

(* Total kinetic energy at specific time *)
TotalKineticEnergy[mass_List, vel_List] := 0.5 mass.(Norm/@vel)^2 /; Depth[mass]==2 && Depth[vel]==3

(* Total kinetic energy for all times *)
TotalKineticEnergy[mass_List, vel_List] := 0.5 * MapThread[#1.(Norm /@ #2)^2&, {mass, vel}] /; Depth[mass]==3 && Depth[vel]==4


Clear[TotalPotentialEnergy]

(* Total potential energy at specific time *)
auxTotalPotentialEnergy1 =
	Compile[{{mass,_Real,1}, {pos,_Real,2}},
		Module[{sum, nmax},
			nmax = Length @ mass;
			sum = 0.;
			Do[
				sum = sum + mass[[i]] mass[[j]]/
				(\[Sqrt]((pos[[i,1]]-pos[[j,1]])^2+(pos[[i,2]]-pos[[j,2]])^2+(pos[[i,3]]-pos[[j,3]])^2)),
				{i, nmax}, {j, i-1}
			];
			-sum
		],
		CompilationTarget -> "C"
	]

(* Total potential energy for all times *)
auxTotalPotentialEnergy2 =
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

TotalPotentialEnergy[mass_, pos_] := auxTotalPotentialEnergy1[mass, pos] /; Depth[mass] == 2 && Depth[pos] == 3
TotalPotentialEnergy[mass_, pos_] := auxTotalPotentialEnergy2[mass, pos] /; Depth[mass] == 3 && Depth[pos] == 4

(* Virial radius at specific time *)
VirialRadius[mass_, pos_] := - 0.5 Total[mass] / TotalPotentialEnergy[mass, pos] /; Depth[mass] == 2 && Depth[pos] == 3

(* Virial radius at all times *)
VirialRadius[mass_, pos_] := - 0.5 (Total/@ mass) / TotalPotentialEnergy[mass, pos] /; Depth[mass] == 3 && Depth[pos] == 4


EnergyFromPairs =
Compile[{{mass,_Real,1},{pos,_Real,2},{vel,_Real,2}},
	Block[{nmax, distance,squaredSpeed,massProduct,list,energy,BIG,aaxis},
		nmax = Length@mass;
		BIG=10.^6;
		squaredSpeed=Table[0.,{i,1,nmax},{j,1,nmax}];
		distance=Table[0.,{i,1,nmax},{j,1,nmax}];
		energy=Table[0.,{i,1,nmax},{j,1,nmax}];
		aaxis=Table[0.,{i,1,nmax},{j,1,nmax}];
			Do[
				If[i!=j,
					distance[[i,j]]=(\[Sqrt]((pos[[i,1]]-pos[[j,1]])^2+(pos[[i,2]]-pos[[j,2]])^2+(pos[[i,3]]-pos[[j,3]])^2));
					squaredSpeed[[i,j]]= ((vel[[i,1]]-vel[[j,1]])^2+(vel[[i,2]]-vel[[j,2]])^2+(vel[[i,3]]-vel[[j,3]])^2);
					massProduct = mass[[i]]mass[[j]];
					energy[[i,j]] = massProduct(0.5/(mass[[i]]+mass[[j]]) squaredSpeed[[i,j]]-1./distance[[i,j]]);
					aaxis[[i,j]] = massProduct/(2 Abs[energy[[i,j]]]);
				],
				{i,1,nmax},{j,1,nmax}
			];
		{energy, distance, squaredSpeed, aaxis}
	],
	CompilationTarget->"C"
];

(* statistical binaries ?*)
FindBinaries[mass_,position_,velocity_] :=
	Module[{pairs, binaries,energy,distance,squaredSpeed,aaxis},
		{energy,distance,squaredSpeed,aaxis} = EnergyFromPairs[mass,position,velocity];
		pairs = Table[ Flatten[{i,Position[energy[[i]],Min[energy[[i]]]]}], {i,1,Length[mass]}];
		binaries = Select[Gather[pairs,#1==Reverse[#2]&], Length[#]==2&][[All,All,1]];
		{binaries, Extract[energy,binaries], Extract[distance,binaries],
			Extract[squaredSpeed,binaries], Extract[aaxis,binaries]}
	];

Clear[TwoBodyProperties];
TwoBodyProperties[{{id1_, mass1_, pos1_, vel1_}, {id2_, mass2_, pos2_, vel2_}}] :=
  	Module[{distance, relativeSpeed, massProduct, energy, majorAxis,
    totalMass, cm, vcm},
   		distance = EuclideanDistance[pos2, pos1];
   		relativeSpeed = EuclideanDistance[vel2, vel1];
   		massProduct = mass1 * mass2;
   		totalMass = mass1 + mass2;
   		energy = massProduct ((0.5 relativeSpeed^2)/(mass1 + mass2 ) - 1./distance);
   		majorAxis = massProduct/(2 Abs[energy]);
   		cm = (mass1 pos1 + mass2 pos2)/totalMass;
   		vcm = (mass1 vel1 + mass2 vel2)/totalMass;
   		{totalMass, distance, relativeSpeed, cm, vcm, majorAxis, energy,
			{id1, mass1, pos1, vel1}, {id2, mass2, pos2, vel2}}
   	];

Clear[InstantaneousMultiples];
InstantaneousMultiples[id_, m_, x_, v_] :=
 	Module[{nearest, mutualNearest, singleRules, pairProperties,
 		negativeEnergyQ, binaries, binariesProperties, starOfSystemQ,
 		id2, m2, x2, v2, nearest1, nearest2, nearest3, binaryRules, triples,
 		triplesProperties, triplesRules, quadruples, quadruplesProperties, name},

  		(*--- Look for binaries ---*)
  		nearest = Nearest[Thread[x -> (name/@id)], x, 2F];
  		mutualNearest =
			Select[Gather[nearest, #1 === Reverse[#2] &], Length[#] == 2 &][[All, All, 1]];
  		singleRules = Thread[(name/@id) -> Transpose[{id, m, x, v}]];
  		pairProperties = TwoBodyProperties /@ (mutualNearest /. singleRules);
  		negativeEnergyQ = Negative /@ pairProperties[[All, 7]];
  		binaries = Pick[mutualNearest, negativeEnergyQ] /. name->Identity;
  		binariesProperties = Pick[pairProperties, negativeEnergyQ];

  		(*--- Look for triples ---*)
  		If[Length[binaries]!=0,
	  		starOfSystemQ = Replace[Replace[id, Thread[Flatten[binaries] -> True], 1], Except[True] -> False, 1];
	  		id2 = Pick[(name/@id), starOfSystemQ, False];
	  		m2 = Pick[m, starOfSystemQ, False];
	  		x2 = Pick[x, starOfSystemQ, False];
	  		v2 = Pick[v, starOfSystemQ, False];
	  		nearest1 = Transpose[{id2, Nearest[Thread[binariesProperties[[All, 4]] -> name/@binaries], x2, 1][[All, 1]]}];
	  		nearest2 = Transpose[{name/@binaries, Nearest[Thread[x2 -> id2], binariesProperties[[All, 4]], 1][[All, 1]]}];
	  		nearest = Join[nearest1, nearest2];
	  		mutualNearest = Select[Gather[nearest, #1 === Reverse[#2] &], Length[#] == 2 &][[All, All, 1]];
	  		binaryRules = Thread[(name/@binaries) -> MapThread[Prepend, {binariesProperties[[All, {1, 4, 5}]], binaries}] ];
	  		pairProperties = TwoBodyProperties /@ (mutualNearest /. binaryRules /. singleRules);
	  		negativeEnergyQ = Negative /@ pairProperties[[All, 7]];
	  		triples = Pick[mutualNearest, negativeEnergyQ] /. name -> Identity;
	  		triplesProperties = Pick[pairProperties, negativeEnergyQ];
  			,
  			triples = {};
  			triplesProperties = {}
  		];

  		(* Look for quadruples *)
  		If[Length[triples]!=0,
  			starOfSystemQ = Replace[Replace[id, Thread[Union[Flatten[Join[binaries, triples]]] -> True], 1], Except[True] -> False, 1];
  			id2 = Pick[name/@id, starOfSystemQ, False];
  			m2 = Pick[m, starOfSystemQ, False];
  			x2 = Pick[x, starOfSystemQ, False];
  			v2 = Pick[v, starOfSystemQ, False];
  			nearest1 = Transpose[{id2, Nearest[Thread[triplesProperties[[All, 4]] -> name/@triples], x2, 1][[All, 1]]}];
  			nearest2 = Transpose[{name/@triples, Nearest[Thread[x2 -> id2], triplesProperties[[All, 4]], 1][[All, 1]]}];
  			nearest3 = If[Length[binaries] > 1, Nearest[Thread[binariesProperties[[All, 4]] -> name/@binaries], binariesProperties[[All, 4]], 2], {}];
  			nearest = Join[nearest1, nearest2, nearest3];
  			mutualNearest = Select[Gather[nearest, #1 === Reverse[#2] &], Length[#] == 2 &][[All, All, 1]];
  			triplesRules = Thread[name/@triples -> MapThread[Prepend, {triplesProperties[[All, {1, 4, 5}]], triples}] ];
  			pairProperties = TwoBodyProperties /@ (mutualNearest /. triplesRules /. binaryRules /. singleRules);
  			negativeEnergyQ = Negative /@ pairProperties[[All, 7]];
  			quadruples = Pick[mutualNearest, negativeEnergyQ]  /. name -> Identity;
  			quadruplesProperties = Pick[pairProperties, negativeEnergyQ];
  			,
  			quadruples = {};
  			quadruplesProperties = {}
  		];

(*
			OLD WAY TO STORE BINARIES
			{{binaries, binariesProperties}, {triples, triplesProperties}, {quadruples, quadruplesProperties}}
*)
			AssociationThread[{"Name", "TotalMass", "Distance", "RelativeSpeed", "RCM", "VCM", "MajorAxis",
				"Energy", "Component1", "Component2"} -> #] & /@
						MapThread[Prepend, {Join[binariesProperties, triplesProperties, quadruplesProperties], Join[binaries, triples, quadruples]}]

  	]

PermanentBinaries[multiples1_, multiples2_] :=
	Module[{binaries, triples},

		binaries = Intersection[multiples1[[1,1]],multiples2[[1,1]]];
		triples = Intersection[multiples1[[2,1]],multiples2[[2,1]]];

		{binaries, triples}
	]

(* ::Subsection:: *)
(*Plots*)

Clear[ScaledListPlot];

Options[ScaledListPlot] = Join[{"ShowAllScales" -> False }, Options[ListPlot]];

ScaledListPlot[list:{{{_?NumberQ, _?NumberQ}..}..}, {scalex_, scaley_}, opts:OptionsPattern[]] :=
	Module[{newlist},

		newlist = Transpose[{scalex, scaley} Transpose[#]] & /@ list;

		ListLinePlot[newlist,
			Sequence @@ FilterRules[{opts}, Options[ListPlot]],
			Frame -> True,
			GridLines -> Automatic]
	];

ScaledListPlot[list:{{{_?NumberQ, _?NumberQ}..}..}, opts:OptionsPattern[]] := ScaledListPlot[list, {1, 1}, opts];

ScaledListPlot[list:{{{_?NumberQ, _?NumberQ}..}..}, scale_, opts:OptionsPattern[]] := ScaledListPlot[list, {$TimeScale, scale}, opts];

ScaledListPlot[list:{{_?NumberQ, _?NumberQ}..}, {scalex_, scaley_}, opts:OptionsPattern[]] :=
	Module[{newlist, ticks, ticksXD, ticksYL, ticksXU, ticksYR},

		If[OptionValue["ShowAllScales"],
			ticks = Ticks /. FullOptions[
												ListPlot[list, ImageSize -> 100,
													Sequence @@ FilterRules[{opts}, Options[ListPlot]]]];
			ticksXD = DeleteCases[ticks[[1]], {_, "", ___}][[All, 1]];
			ticksYL = DeleteCases[ticks[[2]], {_, "", ___}][[All, 1]];
			(* ticksXU = Transpose[{ticksXD, Internal`StringToDouble[ToString[NumberForm[#, {8, 1}]]] & /@ (ticksXD scalex)}];
			ticksYR = Transpose[{ticksYL, Internal`StringToDouble[ToString[NumberForm[#, {8, 1}]]] & /@ (ticksYL scaley)}]; *)
			ticksXU = Transpose[{ticksXD, Round[#, 1.0] & /@ (ticksXD scalex)}];
			ticksYR = Transpose[{ticksYL, # & /@ (ticksYL scaley)}];
			ListLinePlot[list,
				Sequence @@ FilterRules[{opts}, Options[ListPlot]],
				Frame -> True,
				FrameTicks -> {{ticksYL, ticksYR}, {ticksXD, ticksXU}},
				GridLines -> Automatic]
			,
			newlist = Transpose[{scalex, scaley} Transpose[list]];
			ListLinePlot[newlist,
				Sequence @@ FilterRules[{opts}, Options[ListPlot]],
				Frame -> True,
				GridLines -> Automatic]
		]
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

Options[ClusterPlot] =
  	Join[{"Proyection" -> {1, 2}, "Scale" -> {1, 1}, "Singles" -> Automatic, "MultipleLabels" -> False,
    		"Doubles" -> Automatic, "Triples" -> Automatic, "Quadruples" -> Automatic}, Options[Graphics]];

ClusterPlot[pos : {{_?NumberQ, _?NumberQ, _?NumberQ} ..},
  opts : OptionsPattern[]] :=
 	Module[{pos2, proy, scale, labels, singles, directives, primitives},

  		proy = OptionValue["Proyection"];
  		scale = OptionValue["Scale"];
  		pos2 = If[scale =!= {1,1}, (scale * #)& /@ pos[[All, proy]], pos[[All, proy]]];
  		singles = OptionValue["Singles"];
  		labels = {{#2, None}, {#1, None}} & @@ (proy /. {1 -> "x", 2 -> "y", 3 -> "z"});

  		If[singles === Automatic,
   			directives = {AbsolutePointSize[2], Blue},
   			directives = singles
   		];

  		primitives = {If[MatchQ[directives, _List], Sequence @@ directives, directives], Point[pos2]};

  		Graphics[primitives, Sequence @@ FilterRules[{opts}, Options[Graphics]],
   				Frame -> True, AspectRatio -> Automatic, FrameLabel -> labels]
  	]

ClusterPlot[ids : {_?NumberQ ..}, pos : {{_?NumberQ, _?NumberQ, _?NumberQ} ..}, opts : OptionsPattern[]] :=
 	Module[{pos2, proy, scale, labels, singles, directives, primitives, defaultDirectives, doubles, triples,
 		cuadruples, multiples, rules, primitives2, primitives3, primitives4, primitives1, primitives0, multipleLabels},

  		proy = OptionValue["Proyection"];
  		scale = OptionValue["Scale"];
  		pos2 = If[scale == {1,1}, pos[[All, proy]], (scale * #)& /@ pos[[All, proy]]];
  		multipleLabels = OptionValue["MultipleLabels"];
  		labels = {{#2, None}, {#1, None}} & @@ (proy /. {1 -> "x", 2 -> "y", 3 -> "z"});
  		singles = OptionValue["Singles"];
  		doubles = OptionValue["Doubles"];
  		triples = OptionValue["Triples"];
  		cuadruples = OptionValue["Quadruples"];
  		defaultDirectives = {AbsolutePointSize[2], Blue};
  		rules = Dispatch[Thread[ids -> pos2]];

  		If[ doubles === Automatic && triples === Automatic && cuadruples === Automatic,
   			If[ MatchQ[singles, {__Rule}],
    				singles = Flatten[Map[If[MatchQ[#[[1]], _List], Thread[#, List, 1], #] &, singles], 1];
    				primitives =
     					Join[defaultDirectives,
     						Flatten /@ (Transpose[{ids, Point /@ pos2}] /. singles /. {_Integer, p_} :> p)]
    				,
    				If[singles === Automatic,
     					directives = defaultDirectives,
     					directives = singles
     				];
    				primitives = {If[MatchQ[directives, _List], Sequence @@ directives, directives], Point[pos2]}
    		]
   			,
   			doubles = doubles /. Automatic -> {};
   			triples = triples /. Automatic -> {};
   			cuadruples = cuadruples /. Automatic -> {};

   			doubles = Flatten[Map[If[MatchQ[#[[1]], x_List /; ArrayDepth[x] > 1], Thread[#, List, 1], #] &, doubles], 1];
   			triples = Flatten[Map[If[MatchQ[#[[1]], x_List /; ArrayDepth[x] > 1], Thread[#, List, 1], #] &, triples], 1];
   			cuadruples = Flatten[Map[If[MatchQ[#[[1]], x_List /; ArrayDepth[x] > 1], Thread[#, List, 1], #] &, cuadruples], 1];

   			multiples = Union @ Flatten[Reverse@Join[doubles[[All, 1]], triples[[All, 1]], cuadruples[[All, 1]]]];

   			If[ multipleLabels,
    				primitives2 = {If[Head[#2] === List, Sequence @@ #2, #2],
    					Sequence @@ ({Line[#], Map[Tooltip[Point[#], ToString[#]] &, #]} & @ Flatten[#1] /. rules)} & @@@ doubles;
    				primitives3 = {If[Head[#2] === List, Sequence @@ #2, #2],
    					Sequence @@ ({Line[#], Map[Tooltip[Point[#], ToString[#]] &, #]} & @ Flatten[#1] /. rules)} & @@@ triples;
    				primitives4 = {If[Head[#2] === List, Sequence @@ #2, #2],
    					Sequence @@ ({Line[#], Map[Tooltip[Point[#], ToString[#]] &, #]} & @ Flatten[#1] /. rules)} & @@@ cuadruples;
    				,
    				primitives2 = {If[Head[#2] === List, Sequence @@ #2, #2],
    					Sequence @@ ({Line[#], Point[#]} & @ Flatten[#1] /. rules)} & @@@ doubles;
    				primitives3 = {If[Head[#2] === List, Sequence @@ #2, #2],
    					Sequence @@ ({Line[#], Point[#]} & @ Flatten[#1] /. rules)} & @@@ triples;
    				primitives4 = {If[Head[#2] === List, Sequence @@ #2, #2],
    					Sequence @@ ({Line[#], Point[#]} & @ Flatten[#1] /. rules)} & @@@ cuadruples;
    			];

   			If[ MatchQ[singles, {__Rule}],
    				singles = Flatten[Map[If[MatchQ[#[[1]], _List], Thread[#, List, 1], #] &, singles], 1];
    				primitives0 = Join[defaultDirectives, Point /@ Complement[ids, multiples] /. rules];
    				primitives1 = {If[Head[#2] === List, Sequence @@ #2, #2], Point @ #1 /. rules} & @@@ singles;
    				primitives = {primitives0, primitives1, primitives2, primitives3, primitives4};
    				,
    				If[singles === Automatic,
     					directives = defaultDirectives,
     					directives = singles
     				];
    				primitives0 = Join[directives, Point /@ ids /. rules];
    				primitives = {primitives0, primitives2, primitives3, primitives4};
    			]
   		];

  		Graphics[primitives,
  			Sequence @@ FilterRules[{opts}, Options[Graphics]],
   				Frame -> True, AspectRatio -> Automatic, FrameLabel -> labels]
  	]

ClusterPlot[mass: {_?NumberQ ..}, pos: {{_?NumberQ, _?NumberQ} ..},
  opts : OptionsPattern[]] :=
 	Module[{},
  		Print["soon"]
  	]

ClusterPlot[ids: {_?NumberQ ..}, mass: {_?NumberQ ..},
  pos : {{_?NumberQ, _?NumberQ} ..}, opts : OptionsPattern[]] :=
 	Module[{},
  		Print["soon"]
  	]

(* -- example with mass --
Graphics[{AbsoluteThickness[0.6],
  MapThread[{#3, Circle[#1, Scaled[#2]]} & ,
       {xs[[time, 1 ;; 100, {1, 2}]], 0.2*bodys[[time, 1 ;; 100]],
         bodys[[300, 1 ;; 100]]*$MassScale /. {x_ /; x < 1 :> Black,
             x_ /; Inequality[1, Less, x, LessEqual, 3] :> Blue,
      x_ /; x > 3 :> Red}}]},
   Frame -> True, PlotRange -> {{-200, 200}, {-200, 200}}]
*)

Options[ClusterPlot3D] = Join[{"Scale" -> {1, 1, 1}, "Window" -> 50, "WindowCenter" -> {0, 0, 0} }, Options[ListPointPlot3D]];

ClusterPlot3D[{pos_, cm_, mcm_}, time_, opts:OptionsPattern[]] :=
	Module[
		{window, windowCenter, plot1, plot2, scale},

		window = OptionValue["Window"];
		windowCenter = OptionValue["WindowCenter"];
		scale = OptionValue["Scale"];

		plot1 = ListPointPlot3D[(scale * #)& /@ pos[[time, All]],
					Sequence@@FilterRules[{opts}, Plot],
					PlotStyle -> PointSize[0.008],
					PlotRange -> {{-window + windowCenter[[1]], window + windowCenter[[1]]},
									{-window + windowCenter[[2]], window + windowCenter[[2]]},
										{-window + + windowCenter[[3]], window + + windowCenter[[3]]}}];

		plot2 = Graphics3D[{AbsolutePointSize[5],
					Red, Point[ scale cm[[time]]],
					Green, Point[ scale mcm[[time]]]}];

		Show[plot1, plot2]
	]


Clear[MinimalistHistogram];
Options[MinimalistHistogram] = Join[{"LineColor"-> Black, "Normalization" -> "Counts"}, Options[Graphics]];

MinimalistHistogram[{}, ___] := Graphics[{}]

MinimalistHistogram[list_, {width_}, opts:OptionsPattern[]] :=
	Module[{min, max, range, bins, histoX, histoY, normalization, area},
		min = Min[list];
		max = Max[list] + width;
		range = Range[min, max, width];
		bins = BinCounts[list, {min, max, width}];
		area = Total[bins]*width;
		normalization = OptionValue["Normalization"];
		bins =
			Which[
				normalization === "Counts", bins,
				normalization === "Max", bins/Max[bins],
				normalization === "PDF", bins/area
			];

		histoX = Riffle[range, range];
		histoY = Join[{0}, Riffle[bins, bins], {0}];

		Graphics[{OptionValue["LineColor"], Line[Transpose[{histoX, histoY}]]}, Sequence@@FilterRules[{opts}, Options[Graphics]],
			Frame -> True, AspectRatio -> 1/GoldenRatio]

	]

ZoomGraphics[graph_Graphics] :=
	With[
		{gr = First[graph],
		opt = DeleteCases[Options[graph], PlotRange -> _],
		plr = PlotRange /. Options[graph, PlotRange],
    	rectangle = {Dashing[Small], Line[{#1, {First[#2], Last[#1]}, #2, {First[#1], Last[#2]}, #1}]} & },

    	DynamicModule[
    	{dragging = False, first, second, range = plr},
    		Panel[
    			EventHandler[
    				Dynamic[
    					Graphics[If[dragging, {gr, rectangle[first, second]}, gr], PlotRange -> range, Sequence @@ opt]
    				],
      				{
      					{"MouseDown", 1} :> (first = MousePosition["Graphics"]),
      					{"MouseDragged", 1} :> (dragging = True; second = MousePosition["Graphics"]),
      					{"MouseUp", 1} :> If[dragging, dragging = False; range = Transpose[{first, second}], range = plr]
      				}
      			]
      		]
      	]
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
		Export["fort.10", data, "Table"];
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
