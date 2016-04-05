(* ::Package:: *)

(* ::Title:: *)
(*Starlab*)


(* ::Subsection:: *)
(*Load Package*)


BeginPackage["AstroTools`Starlab`", {"AstroTools`Utilities`"}]


(* Parameters *)
$MeanSquareVelocity;
$ClusterSize;
$ClusterMass

$MassScale;
$LengthScale;
$TimeScale;
$EnergyScale;

(* Read from files *)
DynToXML::usage;
DynToXML2::usage;
ReadDynDynamics::usage;
ReadStarlabLogs::usage;

(* Read from XML *)
ExtractTopLevelLog::usage;
ExtractTopLevelDynamics::usage;
ExtractParticles::usage;
DeleteTopLevelLogsAndDynamics::usage;
DeleteEmptyElements::usage;
DeleteLogDynAndEmptyElements::usage;
ReadDataFromXML::usage;

(* Calculations *)
ExtractTopLevelsFromData::usage;
AccumulateFromCMNodes::usage;
ExtractLeavesFromData::usage;
ReadStarlabLogs::usage;


(* Equivalence relations *)
$MetersToParsec = 3.240779289469756`*^-17;
$ParsecToAU = 206264.8062454803`;
$SecondToMegaYear = 3.1709791983764584`*^-14;
$JouleToErgs = 10^7;


(* Astrophysical paramenters *)
$GravitationalConstant = 6.67 * 10^-11;
$SolarMass = 1.988 * 10^30;

(* Other symbols *)
ConversionFunction;


Begin["`Private`"]


(* ::Subsection:: *)
(*Parameters*)


(* Cluster paramenters *)
$MeanSquareVelocity = 25. * 10^6;(* m^2/s^2*)
$ClusterSize = 10^3;

$ClusterMass := $ClusterSize $SolarMass;
$ClusterEnergy := -(1/2)$ClusterMass $MeanSquareVelocity;


(* Scaling factors *)
$MassScale := $ClusterMass;
$LengthScale := -(($GravitationalConstant $ClusterMass^2)/(4 $ClusterEnergy)) $MetersToParsec;
$TimeScale := ($GravitationalConstant $ClusterMass^(5/2))/(-4 $ClusterEnergy)^(3/2) $SecondToMegaYear;
$EnergyScale := $ClusterMass ($LengthScale/$TimeScale)^2 $JouleToErgs;


(* ::Subsection:: *)
(*Read from files*)


convertStoryLine[string_] := 
	Module[{parts},
		If[(!StringFreeQ[string, "===>"]) \[Or] StringMatchQ[string, RegularExpression["[\\s]{7,7}.+"]],
			Return[string]
		];
		parts = StringSplit[string, "=", 2];
		"<pm " <> parts[[1]] <> "= \"" <> parts[[2]] <> "\" />"
	]


(*convertStoryLine2[string_] := 
	Module[{parts},
		If[(!StringFreeQ[string, "===>"]) \[Or] StringMatchQ[string, RegularExpression["[\\s]{7,7}.+"]],
			Return["\""<>string<>"\""]
		];
		parts = StringSplit[string, "=", 2];
		"XMLElement[\"pm\", { \""  <> parts[[1]] <> "\"->\"" <> parts[[2]] <> "\"}, {}],"
	]*)


convertStoryLine3[string_] := 
	Module[{parts},
		If[(!StringFreeQ[string, "===>"]) \[Or] StringMatchQ[string, RegularExpression["[\\s]{7,7}.+"]],
(*			Return["\"" <> string  <> "\""]*)
			Return[0];
		];
		parts = StringSplit[string, "=", 2];
		"\"" <> parts[[1]] <> "\"" <> "->" <> "\"" <> parts[[2]] <> "\"" <> ","
	]


SetAttributes[DynToXML, HoldFirst];

Options[DynToXML] = {Path -> Automatic}

DynToXML[data_, file_, opts:OptionsPattern[]]:=
	Module[
		{lines, line, strm, dir, k},
		
		dir = OptionValue[Path] /. Automatic -> Directory[];
		
		SetDirectory[dir];
		
		Print["start: ", TimeAndMem[]];

		strm = OpenWrite["temp.xml"];

		WriteString[strm, "<?xml version=\"1.0\"?>\n<System>"];	

		lines = ReadList[file, "String" ];	
		
		Print["Number of lines: ", Length[lines]];
		
		
		Do[
			line = lines[[k]];
			If[Mod[k, 1000000]===0, Print["line number: ", k]];
			Which[
				StringMatchQ[line, RegularExpression["[(].+"]], 
					WriteString[strm, StringReplacePart[line, "<", {1,1}] <> ">" ],
				StringMatchQ[line, RegularExpression["[)].+"]],
					WriteString[strm, StringReplacePart[line, "</", {1,1}] <> ">" ],
				True, 
					WriteString[strm, convertStoryLine[line] ]
				], 
				{k, 1, Length[lines]} 
			];
		
		WriteString[strm, "</System>"];
		Close[strm];
		
		Print["Clearing data: ", TimeAndMem[]];
		Clear[lines];

		Print["Importing XML: ", TimeAndMem[]];
		data = Import["temp.xml", "XML"][[2,3]];
		(*ImportString[data, "XML"]*)

		DeleteFile["temp.xml"];
		Print["end: ", TimeAndMem[]];
		ResetDirectory[];
		Return[0]

	]


(*
SetAttributes[DynToXML, HoldFirst];

DynToXML[data_,file_]:=
	Module[
		{lines, line, XMLlines, alllines},
		
		Print["start: ", TimeAndMem[]];

		XMLlines = {"<?xml version=\"1.0\"?>\n<System>"};	

		lines = ReadList[file, "String" ];	
		
		Print["Number of lines: ", Length[lines]];
		
		alllines = 
			Reap[
				Do[
					line = lines[[k]];
					If[Mod[k, 1000000]===0, Print["line number: ", k]];
					Which[
						StringMatchQ[line, RegularExpression["[(].+"]], 
							Sow[ StringReplacePart[line, "<", {1,1}] <> ">" ],
						StringMatchQ[line, RegularExpression["[)].+"]],
							Sow[ StringReplacePart[line, "</", {1,1}] <> ">" ],
						True, 
							Sow[ convertStoryLine[line] ]
					], 
					{k, 1, Length[lines]} 
				]
			][[-1,-1]];
		
		Print["Exporting XML: ", TimeAndMem[]];
		Export["temp.xml", StringJoin[XMLlines, alllines, {"</System>"} ], "TEXT"];
		
		Print["Clearind data: ", TimeAndMem[]];
		Clear[alllines];

		Print["Importing XML: ", TimeAndMem[]];
		data = Import["temp.xml", "XML"][[2,3]];
		(*ImportString[data, "XML"]*)

		Null;
		Print["end: ", TimeAndMem[]];

	]*)


SetAttributes[DynToXML2, HoldFirst];

DynToXML2[data_,file_]:=
	Module[
		{lines, line, strm, str},
		
		Print["start: ", TimeAndMem[]];

		strm = OpenWrite["temp.m"];

		WriteString[strm, "{"];	

		lines = ReadList[file, "String" ];	
		
		Print["Number of lines: ", Length[lines]];
		
		Do[
			line = lines[[k]];
			If[Mod[k, 1000000]===0, Print["line number: ", k, " ", TimeAndMem[]]];
			Which[
				StringMatchQ[line, RegularExpression["[(].+"]], 
					WriteString[strm, "element[\"" <> StringDrop[line, 1] <> "\","],
				StringMatchQ[line, RegularExpression["[)].+"]],
					WriteString[strm, "]," ],
				True, 
					With[{res = convertStoryLine3[line]}, 
						If[res!=0, WriteString[strm, res]]
					]
				], 
			{k, 1, Length[lines]} 
		];
		Print@TimeAndMem[];
		
		WriteString[strm, "}"];
		Close[strm];
		
		Print@TimeAndMem[];
		str = ReadString["temp.m"];
		Print@TimeAndMem[];
		str = StringReplace[str, ",]" -> "]" ];
		Print@TimeAndMem[];
		str = StringReplacePart[str, "}", {-2,-1} ];

		Print["end: ", TimeAndMem[]];
		data = ToExpression[str];
		(*data = Get[StringToStream[str]];*)

		Return[0]

	]


ReadDynDynamics[file_] :=
	Module[
		{lines, alllines, strm, line},

		Print["start: ", PrintTimeAndMem[]];

		lines = ReadList[file, "String" ];

		strm = OpenWrite[FormatType -> OutputForm];

		alllines = 
			Reap[
				Do[
					line = lines[[k]];
					If[Mod[k, 1000000]===0, Print["line number: ", k]];
					Which[
						StringMatchQ[line, RegularExpression["(\\(Particle)$"]], 
							Sow["{"],
						StringMatchQ[line, RegularExpression["(\\(Dynamics)$"]], 
							Sow["{"],
						StringMatchQ[line, RegularExpression["(\\)Particle)$"]],
							Sow["},"],
						StringMatchQ[line, RegularExpression["(\\)Dynamics)$"]],
							Sow["},"],
						True, 
							Sow[ convertStoryLine3[line] ]
(*						StringMatchQ[line, RegularExpression["(\\s\\sr\\s\\s=).+"]],
							Sow["StringToNumbers @ "<>"\""<>StringTake[line, {7,-1}]<>"\""]*)
					], 
					{k, 1, Length[lines]} 
				]
			][[-1,-1]];
	
		alllines = StringJoin["{", alllines, "}"];

		Print["end: ", PrintTimeAndMem[]];
		
		StringReplace[alllines, ",}" -> "}" ]

	]


(*SetAttributes[DynToXML, HoldFirst];

DynToXML[data_,file_]:=
	Module[
		{lines, line, newline, XMLlines, alllines, strm},
		
		Print["start: ", PrintTimeAndMem[]];

		strm = OpenWrite["temp.m"];

		WriteString[strm, "{"];	

		lines = ReadList[file, "String" ];	
		
		Print["Number of lines: ", Length[lines]];
		
		
		Do[
			line = lines[[k]];
			If[Mod[k, 1000000]===0, Print["line number: ", k]];
			Which[
				StringMatchQ[line, RegularExpression["[(].+"]], 
					WriteString[strm, StringReplacePart[line, "XMLElement[", {1,1}] <> ",{}, {" ],
				StringMatchQ[line, RegularExpression["[)].+"]],
					WriteString[strm, "}]," ],
				True, 
					WriteString[strm, convertStoryLine2[line] ]
				], 
				{k, 1, Length[lines]} 
			];
		
		WriteString[strm, "}"];
		Close[strm];
		
		Print["Clearind data: ", PrintTimeAndMem[]];
		Clear[lines]

		Print["Importing XML: ", PrintTimeAndMem[]];
		data = Get["temp.m"];
		(*ImportString[data, "XML"]*)

		Null;
		Print["end: ", PrintTimeAndMem[]];

	]*)


ReadStarlabLogs[file_, stringPattern_, filter_:Identity]:=
	Module[{lines,data},
		lines = ReadList[file,"String"];
		data = filter@Flatten[StringCases[lines, stringPattern]];
		StringToNumbers /@ data
]


ReadStarlabLogs[2][file_, stringPattern_, filter_:Identity]:=
	Module[{lines, len, i, str2},
		lines = ReadList[file, "String"];
		len = Length[lines];
		i = 1;

		Reap[
		While[ i <= len, 
			If[StringMatchQ[lines[[i]], "Time ="~~__],
				++i;
				While[!StringMatchQ[lines[[i]], __~~"virial_ratio"~~__],
					str2 = StringCases[lines[[i]], stringPattern];
					If[str2 =!= {}, Sow[StringToNumbers@@str2]];
					++i
				],
				++i
			]
		]][[-1, -1]]
]


ReadStarlabLogs[file_, stringPattern_, "TEST", filter_:Identity]:=
	Module[{lines, k , str, str2, reap},
		lines = ReadList[file, "String"];
		
		k = Length[lines];
		str = lines[[k]];
		
		(* make sure to skip nn bounds *)
		While[!StringMatchQ[str, "  Total binary energy"~~___],
			k = k - 1;
			str = lines[[k]]
		];

		reap = 
		Reap[
			While[!StringMatchQ[str, "Time ="~~___],
				str2 = StringCases[str, stringPattern];
				If[str2 =!= {}, Sow[StringToNumbers@@str2]];
				k = k - 1;
				str = lines[[k]]
			]
		][[-1]];
		If[reap === {}, Print["No pattern in: ", file]; None, reap[[1]] ]
]

(* SLOW CODE :
    ReadStarlabLogs[file_, stringPattern_, "TEST", filter_:Identity]:=
    Module[{data, k , str, str2, strm, pos},
      strm = OpenRead[file];

      pos = Reap[
              While[Skip[strm, String] =!= EndOfFile,
                Sow[StreamPosition[strm]]
              ]
            ][[-1, -1]];

      k = -2;
      SetStreamPosition[strm, pos[[k]]];

      str = Read[strm, String];

      data = Reap[
        While[!StringMatchQ[str, "Time ="~~___],
          str2 = StringCases[str, stringPattern];
          If[str2 =!= {}, Sow[StringToNumbers@@str2]];
          k = k - 1;
          SetStreamPosition[strm, pos[[k]]];
          str = Read[strm, String];
        ]
      ][[-1,-1]];

      Close[strm];
      data

    ]*)

(* SLOW CODE :
ReadStarlabLogs[file_, stringPattern_, "TEST", filter_:Identity]:=
    Module[{data, k , str, str2, strm, pos},
      strm = OpenRead[file];

      str = Read[strm, String];
      While[!StringMatchQ[str, "Time = 1000"~~___],
        str = Read[strm, String];
      ];
      Print[str];
      data = Reap[
        While[str =!= EndOfFile,
          str2 = StringCases[str, stringPattern];
          If[str2 =!= {}, Sow[StringToNumbers@@str2]];
          str = Read[strm, String];
        ]
      ][[-1,-1]];

      Close[strm];
      data

    ]
*)



(* ::Subsection:: *)
(*Read from XML*)


ExtractTopLevelLog[data_] := Cases[#, XMLElement["Log", __], {2}]& /@ data
ExtractTopLevelDynamics[data_] := Cases[#, XMLElement["Dynamics", __], {2}]& /@ data
ExtractParticles[data_] := Cases[#, XMLElement["Particle", __], {2}]& /@ data


DeleteTopLevelLogsAndDynamics[data_] := DeleteCases[#, XMLElement["Log"|"Dynamics", __], {2}]& /@ data


DeleteEmptyElements[data_] := DeleteCases[#, XMLElement["Log"|"Hydro"|"Star", {}, {}], \[Infinity]]& /@ data


DeleteLogDynAndEmptyElements[data_] := DeleteEmptyElements[DeleteTopLevelLogsAndDynamics[data]]

Options[ReadDataFromXML] = {ConversionFunction -> StringToNumbers}

ReadDataFromXML[data_, var_String, opts:OptionsPattern[]] :=
	Module[{readXMLElement, func},

		func = OptionValue[ConversionFunction];
		readXMLElement[XMLElement["Particle", {}, list_]] := readXMLElement /@ list;
		readXMLElement[XMLElement["Dynamics", {}, list_]] := readXMLElement /@ list;	
		readXMLElement[XMLElement["pm", {var -> r_}, {}]] := func[r];
		readXMLElement[XMLElement[__]] := Sequence[];

		Map[readXMLElement, data, {2}]
	
	];

(* Obsolete slower versions *)

(*
readDataFromXML[data_] := 
	(# //. XMLElement["Particle"|"Dynamics",{},list_] \[RuleDelayed] list /. 
			XMLElement["pm", {"r" \[Rule] r_}, {}] \[RuleDelayed] StringToNumbers[r] /. 
			XMLElement["pm", __] \[RuleDelayed] Sequence[]) &/@ data;

readDataFromXML[data_] := 
	(Replace[#, XMLElement["Particle"|"Dynamics", {}, list_] \[RuleDelayed] list,\[Infinity]] /. 
		XMLElement["pm", {"r" \[Rule] r_}, {}] \[RuleDelayed] StringToNumbers[r] /. 
		XMLElement["pm", __]\[RuleDelayed]Sequence[])& /@ data;
*)

(* Read all data > to be tested wiht extract XML functions *)
ReadDataFromXML[data_] :=
	Module[{readXMLElement},
	    readXMLElement[XMLElement["Particle", {}, list_]] := readXMLElement /@ list;
		readXMLElement[XMLElement["Dynamics", {}, list_]] := readXMLElement /@ list;	
		readXMLElement[XMLElement["pm", {"m" -> m_}, {}]] := StringToNumbers[m];
	    readXMLElement[XMLElement["pm", {"r" -> r_}, {}]] := StringToNumbers[r];
	    readXMLElement[XMLElement["pm", {"v" -> v_}, {}]] := StringToNumbers[v];
		readXMLElement[XMLElement[__]] := Sequence[];

		Map[readXMLElement, data, {2}]	
	]



(* ::Subsection:: *)
(*Calculations*)


ExtractTopLevelsFromData[data_, obj_] := 
	Which[
		obj == "scalar",
			Map[First[First[#]]&, data, {2}],
		obj == "vector",
			Null (* ---  *)
	]


ExtractLeavesFromData[data_, obj_] := 
	Which[
		obj == "scalar",
			Flatten[Level[#,{-3}],2]& /@ data,
		obj == "vector",
			Flatten[Level[#,{-4}],1]& /@ data
	]


AccumulateFromCMNodes[data_] := 
	Module[{acummulate},
	
		acummulate[{r1_List}] := r1;
		acummulate[{r1_List, rs__List}] := acummulate /@ MapAt[r1+#&, {rs}, {All, 1}];

		Flatten[#, 1]& /@ Map[ Level[acummulate[#], {-2}]&, data, {2}]
		
	]


(* ::Subsection:: *)
(*End*)


End[ ]


EndPackage[ ]
