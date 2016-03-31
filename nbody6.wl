(* ::Package:: *)

(* ::Title:: *)
(*nbody6*)


(* ::Subsection:: *)
(*LoadPackage*)


BeginPackage["AstroTools`nbody6`", {"AstroTools`Utilities`"}]


ReadOutput::usage = ""
ReadOUT3::usage = ""
ReadOUT33::usage = ""
ReadESC::usage = ""


KSTART::usage = "Control index (1: new run; >1: restart; 3, 4, 5: new params).";
TCOMP::usage = "Maximum CPU time in minutes (saved in CPU).";
\[ScriptCapitalN]::usage = "Number of objects (N_s + 2*N_b; singles + 3*NBIN0 < NMAX)";
NFIX::usage = "Output frequency of data save or binaries (options 3 & 6)."
NCRIT::usage = "Final particle number (alternative termination criterion)."
NRAND::usage = "Random number sequence skip."
NNBMAX::usage = "Maximum number of neighbours (< LMAX - 5)."
NRUN::usage = "Run identification index."
ETAI::usage = "Time-step parameter for irregular force polynomial."
ETAR::usage = "Time-step parameter for regular force polynomial."
RS0::usage = "Initial radius of neighbour sphere (N-body units)."
DTADJ::usage = "Time interval for parameter adjustment (N-body units)."
DELTAT::usage = "Output time interval (N-body units)."
TCRIT::usage = "Termination time (N-body units)."
QE::usage = "Energy tolerance (restart if DE/E > 5QE & KZ(2) > 1)."
RBAR::usage = "Virial cluster radius in pc (set = 1 for isolated cluster)."
ZMBAR::usage = "Mean mass in solar units (=1.0 if 0; final depends on #20)."
DTMIN::usage = "Time-step criterion for regularization search."
RMIN::usage = "Distance criterion for regularization search."
ETAU::usage = "Regularized time-step parameter (6.28/ETAU steps/orbit)."
ECLOSE::usage = "Binding energy per unit mass for hard binary (positive)."
GMIN::usage = "Relative two-body perturbation for unperturbed motion."
GMAX::usage = "Secondary termination parameter for soft KS binaries."

KZ01::usage =  "COMMON save unit 1 (=1: 'touch STOP'; =2: every 100NMAX steps)."
KZ02::usage =  "COMMON save unit 2 (=1: at output; =2: restart if DE/E > 5QE)."
KZ03::usage =  "Basic data unit 3 at output time (unformatted, frequency NFIX;
             =1/2: standard and tail; =3: tail only; >3: cluster + tail)."
KZ04::usage =  "Binary diagnostics on unit 4 (# threshold levels = KZ(4) < 10);
                                       (suppressed in input.f & ksint.f);
                                 new usage: number of NS & BH on unit #4;
                                        >1: BH mass histogram."
KZ05::usage =  "Initial conditions (#22 =0; =0: uniform & isotropic sphere);
                =1: Plummer; =2: two Plummer models in orbit, extra input;
                =3: massive perturber and planetesimal disk, extra input;
                =4: massive initial binary, extra input: A, E, M1, M2;
                =5: Jaffe model;
               >=6: Zhao BH cusp model, extra input if #24 < 0: ZMH, RCUT."
KZ06::usage =  "Soft & regularized binaries & individual bodies at main output;
                =1: soft & regularized binaries on unit 6;
                =2: regularized binaries only;
                >2: individual bodies (loop from 1 to KZ(6))."
KZ07::usage =  "Lagrangian radii (>0: RSCALE; =2, 3, 4: output units 6, 7);
                >=2: half-mass radii of 50% mass, also 1% heavies, unit 6;
                >=2: Lagrangian radii for two mass groups on unit 31 & 32;
                >=2: geometric radii for three mass groups on unit 6;
                 =5: density, rms velocity & mean mass on unit 26, 27 & 36;
                 =6: pairwise values of mean mass and radii on unit 28."
KZ08::usage =  "Primordial binaries (=1 & >=3 routine BINPOP; >=3: SWEEP;
                               =4: Kroupa 1995 period distribution;
                               >4: standard setup using RANGE & SEMI0)."
KZ09::usage =  "Binary output  (=1, 2, 3 in BINDAT):
                         =1: regularized binaries on OUT9;
                         >1: hierarchical systems on HIDAT (NMERGE > 0);
                         =2: regularized and soft binaries (unit #19);
                         =3: soft binaries only on #19."
KZ10::usage =  "Diagnostic KS output (>0: begin KS; >1: end; >=3: each step)."
KZ11::usage =  "Algorithmic Chain regularization and post-Newtonian (NBODY7).
              non-zero: PN for unpert KS or re-init ARChain (ksint.f);
              > 0: addition of initial BHs (binary/singles; scale.f);
              = -1: standard case of subsystem for ARChain (ksint.f);
              < -1: ARChain restricted to BH binary components (ksint.f)."
KZ12::usage =  "HR diagnostics of evolving stars (> 0; interval DTPLOT);
               =2: input of stellar parameters on fort.12 (routine INSTAR)."
KZ13::usage =  "Interstellar clouds (=1: constant velocity; >1: Gaussian)."
KZ14::usage =  "External force (=1: standard tidal field; =2: point-mass galaxy;
              =3: point-mass + bulge + disk + halo + Plummer; =4: Plummer)."
KZ15::usage =  "Triple, quad, chain (#30 > 0) or merger search (>1: more output)."
KZ16::usage =  "Updating of regularization parameters (>0: RMIN, DTMIN & ECLOSE);
                  >1: RMIN expression based on core radius;
                  >2: modify RMIN for GPERT > 0.05 or < 0.002 in chain."
KZ17::usage =  "Modification of ETAI, ETAR (>=1) and ETAU (>1) by tolerance QE."
KZ18::usage =  "Hierarchical systems (=1: diagnostics; =2: primordial; =3: both)."
KZ19::usage =  "Mass loss (=1: old supernova scheme; =3: Eggleton, Tout & Hurley;
                                               >3: extra diagnostics)."
KZ20::usage =  "Initial mass function (=0: Salpeter type using ALPHAS; =1: Scalo;
              =2, 4: Kroupa 1993; =3, 5: Eggleton; > 1: primordial binaries;
              =6, 7: Kroupa 2001; binary correlated m1/m2, also brown dwarfs.
              Note: Use PARAMETER (MAXM=1) for setting BODY(1) = BODY10).
              KGT93 (Kroupa, Gilmore & Tout 1993) not recommended."
KZ21::usage =  "Extra output (>0: MODEL #, TCOMP, DMIN, AMIN; >1: NESC by JACOBI)."
KZ22::usage =  "Initial m, r, v on #10 (=1: output; >=2: input; >2: no scaling;
              =2: m, r, v on #10 in any units; scaled to standard units;
                  Note: choose #20 = 0 to avoid Salpeter IMF with scaling;
              =3: no scaling of input read on fort.10;
              =4: input from mcluster.c (no scaling; binaries if NBIN0 >0);
              =-1: astrophysical input (M_sun, km/s, pc) on unit #10)."
KZ23::usage =  "Escaper removal (>1: diagnostics in file ESC with V_inf in km/s);
                           >=3: initialization & integration of tidal tail."
KZ24::usage =  "Initial conditions for subsystem (M,X,V routine SCALE; KZ(24)= #);
                           <0: ZMH & RCUT (N-body units) Zhao model (#5>=6)."
KZ25::usage =  "Velocity kicks for white dwarfs (=1: type 11 & 12; >1: all WDs)."
KZ25::usage =  "Partial reflection of KS binary orbit (GAMMA < GMIN; suppressed)."
KZ26::usage =  "Slow-down of two-body motion (>=1: KS; >=2: chain; =3: rectify)."
KZ27::usage =  "Tidal effects (=1: sequential; =2: chaos; =3: GR energy loss);
                         =-1: collision detector, no coalescence, #13 < 0."
KZ28::usage =  "GR radiation for NS & BH binaries (with #19 = 3; choice of #27);
                         =4 and #27 = 3: neutron star capture (instar.f)."
KZ29::usage =  "Boundary reflection for hot system (suppressed)."
KZ30::usage =  "Multiple regularization (=1: all; >1: BEGIN/END; >2: each step);
                                =-1: CHAIN only; =-2: TRIPLE & QUAD only." 
KZ31::usage =  "Centre of mass correction after ADJUST (don't use with #23 = 0)."
KZ32::usage =  "Increase output intervals & SMAX based on single particle energy."
KZ33::usage =  "Histograms at main output (>=1: STEP; =2: STEPR, NBHIST & BINARY)."
KZ34::usage =  "Roche-lobe overflow (=1: ROCHE & SYNCH; =2: ROCHE & BSE synch)."
KZ35::usage =  "Time offset (global time from TTOT = TIME + TOFF; offset = 100)."
KZ36::usage =  "Step reduction for hierarchical systems (suppressed)."
KZ37::usage =  "Neighbour additions in CHECKL (>0: high-velocity; >1: all types)."
KZ38::usage =  "Force polynomial corrections (=0: standard, no corrections;
                                =1: all gains & losses included;
                                =2: small FREG change skipped;
                                =3: fast neighbour loss only)."
KZ39::usage =  "No unique density centre (skips velocity modification of RS(I))."
KZ40::usage =  "Neighbour number control (=1: increase if <NNB>  <  NNBMAX/2);
                     >=2: fine-tuning at NNBMAX/5; =3: reduction of NNBMAX."
KZ41::usage =  "Pre-mainsequence stellar evolution (only solar metallicity)."
KZ42::usage =  "Kozai diagnostics on fort.42 (=1: frequency 100 & EMAX > 0.99)."
KZ43::usage =  "Small velocity kick after GR coalescence (=1, =3; NBODY7 only),
                         =2: BH accretion of disrupted star, KSTAR >= 10.
                        >=2: disrupted star ejected as ghost, KSTAR < 10."
KZ44::usage =  "Plotting file for main cluster parameters on fort.56 (OUTPUT)."
KZ45::usage =  "Plotting file for BH (NAME = 1 or 2) on unit 45 (routine BHPLOT);
                      primordial BH defined by INSTAR; membership = KZ(24);
                          =1: plotting output for BH (one or two);
                          >1: BH in KS binary (NCH = 0, unit 45);
                          >2: KS & perturber diagnostics (E > 0.9, EX > 0.9);
                          >3: output for 2nd innermost BH orbit (unit #49);
                          <0: strong three-body events (impact.f, unit #49)."
KZ46::usage =  "Reserved for data analysis project on NBODY6++."
KZ47::usage =  "Reserved for data analysis project on NBODY6++."
KZ48::usage =  "Three-body stability comparison (Valtonen & Mardling 2008 fort.88)."
KZ49::usage =  "Post-Newtonian perturbations included in KS (dir Block)."
KZ50::usage =  "Not used."


ALPHAS::usage = "Power-law index for initial mass function (used if #20 < 2)."
BODY1::usage = "Maximum particle mass before scaling (KZ(20): solar mass)."
BODYN::usage = "Minimum particle mass before scaling."
NBIN0::usage = "Number of primordial binaries (for IMF2 with KZ(20) > 1)."
NHI0::usage = "Primordial hierarchies (may be needed in IMF if > 0)."
ZMET::usage = "Metal abundance (in range 0.03 - 0.0001)."
EPOCH0::usage = "Evolutionary epoch (in 10**6 yrs; NB! < 0 for PM evolution)."
DTPLOT::usage = "Plotting interval for HRDIAG (N-body units; >= DELTAT)."
Q::usage = "Virial ratio (Q = 0.5 for equilibrium)."
VXROT::usage = "XY-velocity scaling factor (> 0 for solid-body rotation)."
VZROT::usage = "Z-velocity scaling factor (not used if VXROT = 0)."
RTIDE::usage = "Unscaled tidal radius (#14 >= 2; otherwise copied to RSPH2)."
SMAX::usage =  "Maximum time-step (factor of 2 commensurate with 1.0)."





Begin["`Private`"]


(* ::Subsection:: *)
(*Info*)


(* ::Input:: *)
(*(*n=2000;*)
(*rv=1.0;*)
(*m=1;*)
(*Print["RS0 = ",s0=0.3(n/1000)^(1/3)];*)
(*Print["NNBMAX = ",nmax=2.n^(1/2)];*)
(*Print["RMIN = ",rcl=4rv/n];*)
(*Print["DTMIN = ",tcl=0.04(rcl^3/m)^(1/2)];*)*)


(* ::Subsection:: *)
(*Input*)


Options[nbody6Input] = {
	KSTART -> "1", TCOMP -> "20.0", 
	\[ScriptCapitalN] -> "100", NFIX -> "1", NCRIT -> "5", NRAND -> "5000", NNBMAX -> "95", NRUN -> "1",
	ETAI -> "0.02", ETAR -> "0.03", RS0 -> "0.3", DTADJ -> "2.0", DELTAT -> "10.0", TCRIT -> "500", QE -> "2\[Times]\!\(\*SuperscriptBox[\(10\), \(-4\)]\)", RBAR -> "1.0", ZMBAR -> "0.5",
	KZ01 -> "0", KZ02 -> "0", KZ03 -> "1", KZ04 -> "0", KZ05 -> "1", KZ06 -> "0", KZ07 -> "6", KZ08 -> "0", KZ09 -> "0", KZ10 -> "0",
	KZ11 -> "0", KZ12 -> "0", KZ13 -> "0", KZ14 -> "2", KZ15 -> "1", KZ16 -> "1", KZ17 -> "0", KZ18 -> "1", KZ19 -> "0", KZ20 -> "0",
	KZ21 -> "1", KZ22 -> "0", KZ23 -> "2", KZ24 -> "0", KZ25 -> "0", KZ26 -> "1", KZ27 -> "0", KZ28 -> "0", KZ29 -> "0", KZ30 -> "2",
	KZ31 -> "0", KZ32 -> "0", KZ33 -> "0", KZ34 -> "0", KZ35 -> "0", KZ36 -> "0", KZ37 -> "0", KZ38 -> "0", KZ39 -> "0", KZ40 -> "1",
	KZ41 -> "0", KZ42 -> "0", KZ43 -> "0", KZ44 -> "0", KZ45 -> "0", KZ46 -> "0", KZ47 -> "0", KZ48 -> "0", KZ49 -> "0", KZ50 -> "0",
	DTMIN -> "10^-5", RMIN -> "\!\(\*SuperscriptBox[\(10\), \(-4\)]\)", ETAU -> "0.2", ECLOSE -> "1.0", GMIN -> "\!\(\*SuperscriptBox[\(10\), \(-6\)]\)", GMAX -> "0.001",
	ALPHAS -> "2.3", BODY1 -> "10.0", BODYN -> "0.2", NBIN0 -> "0", NHI0 -> "0", ZMET -> "0.02", EPOCH0 -> "0", DTPLOT -> "10.0", 
	Q -> "0.5", VXROT -> "0", VZROT -> "0", RTIDE -> "0", SMAX -> "0.125"
}


nbody6Input[file_, opts:OptionsPattern[]] := 
	Module[{}
		
		strm = OpenWrite[];
		
		Close[strm];
	]


(* ::Subsection:: *)
(*Read Ouput*)


ReadOutput[file_] := 
	Module[{strm, string, string1, template1, read1}, 
		strm = OpenRead["output"];
		string = ReadList[strm, String];
		Close[strm];

		string1 = DeleteCases[StringCases[string, "#1"~~x___:>x], {}]; 
		template1 = {5,6,6,7,5,7,6,7,6,6,7,5,6,8,8,9,7,7,6,6,6,6};
		template1 = {1,0} + #& /@ Partition[Prepend[Accumulatex[template1], 0], 2, 1];
		read1 = StringToNumbers /@ First@StringTake[#, template1]&;
		read1 /@ string1

	]


ReadOUT3[file_] := 
	Module[{strm, NTOT, MODEL, NRUN, NK, endOfFile, reap},

		strm = OpenRead[file, BinaryFormat -> True];
		endOfFile = BinaryRead[strm, Table["Byte", {4}]];

		reap =  
			Reap[
				While[Last[endOfFile] =!= EndOfFile, 			
					Sow[{NTOT, MODEL, NRUN, NK} = BinaryRead[strm, Table["Integer32", {4}]], "VARS"];
					BinaryRead[strm, Table["Byte", {8}]];
					Sow[BinaryRead[strm, Table["Real32", {NK}]], "AS"];
					Sow[BinaryRead[strm, Table["Real32", {NTOT}]], "BODYS"];
					Sow[BinaryRead[strm, Table["Real32", {3*NTOT}]], "XS"];
					Sow[BinaryRead[strm, Table["Real32", {3*NTOT}]], "VS"];
					Sow[BinaryRead[strm, Table["Integer32",{NTOT}]], "NAME"];
					BinaryRead[strm, Table["Byte", {4}]];
					endOfFile = BinaryRead[strm, Table["Byte", {4}]];
				],
			{"VARS", "AS", "BODYS", "XS", "VS", "NAME"}
		];

		Close[strm];
		reap[[-1]]
	]


readOUT33[file_] := 
	Module[{strm, NTAIL, MODEL, NK, endOfFile, reap},

		strm = OpenRead[file, BinaryFormat -> True];
		endOfFile = BinaryRead[strm, Table["Byte", {4}]];

		reap =  
			Reap[
				While[Last[endOfFile] =!= EndOfFile, 			
					Sow[{NTAIL, MODEL, NK} = BinaryRead[strm, Table["Integer32", {4}]], "VARS"];
					BinaryRead[strm, Table["Byte", {8}]];
					Sow[BinaryRead[strm, Table["Real32", {NK}]], "AS"];
					Sow[BinaryRead[strm, Table["Real32", {NTAIL}]], "BODYS"];
					Sow[BinaryRead[strm, Table["Real32", {3*NTAIL}]], "XS"];
					Sow[BinaryRead[strm, Table["Real32", {3*NTAIL}]], "VS"];
					Sow[BinaryRead[strm, Table["Integer32",{NTAIL}]], "NAME"];
					BinaryRead[strm, Table["Byte", {4}]];
					endOfFile = BinaryRead[strm, Table["Byte", {4}]];
				],
			{"VARS", "AS", "BODYS", "XS", "VS", "NAME"}
		];

		Close[strm];
		reap[[-1]]
	]


(* ::Subsection:: *)
(*End*)


End[ ]


EndPackage[ ]
