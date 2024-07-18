(*

:Context: PlaneCurveFormulas
:Title: PlaneCurveFormulas
:Author: Xah Lee
:Copyright: 1995, 2024 by Xah Lee.
:Date: 2024-02-19

:URL: http://xahlee.info/M/plane_curve_formulas.html

:Package Version: 2.1.20240717212814

:Discussion:

This package is useful for studing special plane curves.
It is a collection of plane curve formulas.
It allows you to type a curve name insteaf of remembering the formula.

*)

BeginPackage["PlaneCurveFormulas`"]

Clear[
ArchimedeanSpiral,
Astroid,
BowditchCurve,
Cardioid,
CassinianOval,
Catenary,
CissoidOfDiocles,
Clothoid,
ConchoidOfNicomedes,
Conics,
CrossCurve,
Cycloid,
Deltoid,
Ellipse,
Epicycloid,
Epitrochoid,
EquiangularSpiral,
FoliumOfDescartes,
Hyperbola,
HyperbolicSpiral,
Hypocycloid,
Hypotrochoid,
LemniscateOfBernoulli,
LemniscateOfGerono,
LimaconOfPascal,
Lituus,
Nephroid,
Parabola,
ParabolicSpiral,
Piriform,
QuadratrixOfHippias,
Rose,
SemicubicParabola,
Serpentine,
Tractrix,
Trisectrix,
TrisectrixOfMaclaurin,
Trochoid,
WitchOfAgnesi
 ];

ArchimedeanSpiral::usage =
"ArchimedeanSpiral[n][t] return a parametrization for Archimedean spiral.
Parameter range: 0 <= t <= Infinity.
n is the parameter in the polar equation r == theta^n.
Example:
ParametricPlot[ Evaluate@ ArchimedeanSpiral[1][t], {t,0, 20 Pi}]
";

Astroid::usage =
"Astroid[][t] return a parametrization of the curve astroid.
Parameter range: 0 <= t < 2 Pi.
Example:
ParametricPlot[ Evaluate@ Astroid[][t], {t, 0, 2 Pi}]
";

BowditchCurve::usage =
"BowditchCurve[m,c][t] return a parametrization of Bowditch curve.
Parameter range: 0 <= t < LCM[1/m 2,2]*Pi.

The curve is non-periodic if m is irrational.
Bowditch curve (aka Lissajous figures)
is the trace of two perpendicular harmonic motions.
Try different combination of m and c.
m = 1/2, 1/3,2/3,3/5,4/5,...
c = 0, Pi/4, Pi/3, Pi/2, Pi...
Examples:

ParametricPlot[ Evaluate@ BowditchCurve[3/5,Pi][t],{t, 0, 10 Pi}]
";

Cardioid::usage =
"Cardioid[][t] return a parametrization of the curve cardioid.
Parameter range: 0 <= t < 2 Pi.
Example:
ParametricPlot[ Evaluate@ Cardioid[][t], {t, 0, 2 Pi}]
";

CassinianOval::usage =
"CassinianOval[a,b][t] return a parametrization of Cassinian curve.
Parameter Range: 0 <= t < 2 Pi.
Note: this parametrization does not work well if a >= b.
Cartesian Equation: ((x-a)^2 + y^2) ((x+a)^2 + y^2) == b^4.
Polar Equation: r^4 + a^4 - 2 r^2 a^2 Cos[2 theta] == b^4.
CassinianOval is defined as the locus of points P such that distance[P,F1]*distance[P,F2] == b^2, where F1 and F2 are two fixed points (foci) 2 a distance apart and b is a constant.
Example:
ParametricPlot[Evaluate[CassinianOval[1, 1][t]], {t, 0, 2 Pi}]
";

Catenary::usage =
"Catenary[][t] return a parametrization for the curve Catenary (aka chainette or alysoid).
Example:
ParametricPlot[ Evaluate@ Catenary[][t], {t,-2,2}]
";

CissoidOfDiocles::usage =
"CissoidOfDiocles[][t] return a parametrization for the curve Cissoid of Diocles.
Parameter Range: -Pi/2 < t < Pi/2.
Cartesian equation: y^2 (1-x) == x^3.
Example:
ParametricPlot[Evaluate@CissoidOfDiocles[][t], {t, -Pi/2, Pi/2}]
";

Clothoid::usage =
"Clothoid[n][t] return a parametrization for the curve Clothoid.
Example:
ParametricPlot[ Evaluate@ Clothoid[1][t], {t,-9,5}]
";

ConchoidOfNicomedes::usage =
"ConchoidOfNicomedes[a,k][t] return a parametrization for the Conchoid of Nicomedes.
Parameter Range: -Pi/2 < t < 3/2 Pi, t != Pi/2.
Example:
ParametricPlot[ Evaluate@ ConchoidOfNicomedes[1,2][t], {t,-1.5,4.6}]
";

Conics::usage =
"Conics[e][t] return a parametrization for conics section with one focus at Origin, eccentricity e, directrix at x==1, and vertexes at {e/(1+e),0} and {e/(-1+e),0}.
Parameter range: 0 <= t < 2 Pi.
Example:
ParametricPlot[ Evaluate[{Conics[.8][t], Conics[1][t], Conics[1.4142][t]}], {t, 0, 2 Pi}]
";

CrossCurve::usage =
"CrossCurve[][t] return a parametrization for the Cross Curve.
The Cartesian for cross curve with a point at Origin added is x^2+y^2-x^2*y^2 == 0.
Example:
ParametricPlot[Evaluate@ CrossCurve[][t],{t,0,2*Pi}]
";

Cycloid::usage =
"Cycloid[][t] return a parametrization for cycloid.
Example:
ParametricPlot[ Evaluate@ Cycloid[][t], {t, 0, 4 Pi}]
";

Deltoid::usage =
"Deltoid[][t] return a parametrization for the curve deltoid.
Parameter range: 0 <= t < 2 Pi.
Example:
ParametricPlot[ Evaluate@ Deltoid[][t], {t, 0, 2 Pi}]
";

Ellipse::usage =
"Ellipse[e][t] return a parametrization for ellipse with center at Origin and eccentricity e.
Periodicy: 0 <= t < 2 Pi.
Focus: {e,0}.
Vertex: {1,0}.
Example:
ParametricPlot[ Evaluate[ Ellipse[.7][t]], {t,0, 2 Pi}]
";

Epicycloid::usage =
"Epicycloid[n][t] return a parametrization for epicycloid of n cusps.
n is an integer greater than 0.
Parameter range: 0 <= t < 2 Pi.
Example:
ParametricPlot[ Evaluate@ Epicycloid[6][t],{t,0, 2 Pi}]
";

Epitrochoid::usage =
"Epitrochoid[a,b,h][t] return a parametrization of epitrochoid curve.
Parameter range: 0 <= t < n 2 Pi.
n is an integer greater than 0.

Epitrochoid is defined as the trace of a point, fixed in position to a circle C, while this circle rolls around a fixed circle.
a is the radius of the fixed circle.
b is the radius of the rolling circle.
h is the distance from the center of the rolling circle to the tracing point.

Example:
ParametricPlot[ Evaluate@ Epitrochoid[1,2/3,1][x], {x,0,2 2 Pi} ]
";

EquiangularSpiral::usage =
"EquiangularSpiral[alpha][t] return a parametrization for equiangular spiral.
Parameter range: 0 <= t < Infinity.
Example:
ParametricPlot[ Evaluate@ EquiangularSpiral[82 Degree][t], {t,0, 10 Pi}]
";

FoliumOfDescartes::usage =
"FoliumOfDescartes[][t] return a parametrization for folium of Descartes.
Parameter range: -Infinity < t < Infinity.
Example:
ParametricPlot[Evaluate@ FoliumOfDescartes[][t], {t,-2,2}]
";

Hyperbola::usage =
"Hyperbola[e][t] return a parametrization for hyperbola with center at Origin and eccentricity e.
Parameter range: 0 < t < 2 Pi, t != Pi.
e is a number greater than 1.
Focus: {e,0}. Vertex:{1,0}.
A Rectangular Hyperbola has eccentricity Sqrt[2].
Example:
ParametricPlot[ Evaluate[ Hyperbola[2][t]], {t,0, 2 Pi}]
";

HyperbolicSpiral::usage =
"Hyperbolic spiral (aka reciprocal spiral)
is defined by the polar equation r == 1/theta.
It is so called because the similarity to the
hyperbola equation x == 1/y.
Example:
ParametricPlot[ Evaluate@ HyperbolicSpiral[][t], {t, 0.5, 10 Pi}]
";

Hypocycloid::usage =
"Hypocycloid[n][t] return a parametrization of hypocycloid of n cusp.
Hypocycloid is defined as the trace of, a point on a circle that
rolls around the inside of a fixed circle.
n is an integer greater than 2. Also see ?Hypotrochoid.
Example:
ParametricPlot[ Hypocycloid[6][t], {t,0, 2 Pi}]
";

Hypotrochoid::usage =
"Hypotrochoid[a,b,h][t] return a parametrization of hypotrochoid curve.
Parameter range: 0 <= t < n 2 Pi. n is an integer greater than 0.

Hypotrochoid is defined as the trace, of a point fixed in position to a circle C, while this circle rolls on the inside, of a fixed circle.
a is the radius of the fixed circle.
b is the radius of the rolling circle.
a>b.
h is the distance from the center of the rolling circle to the tracing point.
Hypotrochoid are also known as spiralgraph.

Example:
ParametricPlot[ Evaluate@ Hypotrochoid[1,7/13,7/13][t], {t,0, 7 2 Pi} ]
";

LemniscateOfBernoulli::usage =
"LemniscateOfBernoulli[][t] return a parametrization for lemniscate of Bernoulli
(aka hyperbolic lemniscate, two leafed rose).
Parameter range: 0 <= t < 2 Pi.

The Cartesian equation is (x^2 + y^2)^2 == (x^2-y^2).
Polar equation is r^2 == Cos[2 theta].
Example:
ParametricPlot[ Evaluate@ LemniscateOfBernoulli[][t] , {t, 0, 2 Pi}]
";

LemniscateOfGerono::usage =
"LemniscateOfGerono[][t] return a parametrization for lemniscate of Gerono
(aka figure eight curve).
Parameter range: 0 <= t < 2 Pi.

The Cartesian equation is x^4 == (x^2-y^2).
Example:
ParametricPlot[ Evaluate@ LemniscateOfGerono[][t], {t,0, 2 Pi}]
";

LimaconOfPascal::usage =
"LimaconOfPascal[r,b][t] return a parametrization for the curve limacon of Pascal.
r > 0, b >= 0. Parameter range: 0 <= t < 2 Pi.

This parametrization is defined as conchoid of a circle of radius r with respect to a point on the circle, and offset {b,-b}.
Cartesian equation is
(x^2 + y^2 -2 r x)^2 == b^2 (x^2 + y^2).
Example:
ParametricPlot[ Evaluate@ LimaconOfPascal[1,1][t], {t, 0, 2 Pi}]
";

Lituus::usage =
"Lituus is defined by the polar equation r == 1/Sqrt[theta].
It is a special case of Archimedean spiral.
Parameter range: 0 < t <= 2 Pi or < Infinity
Example:
ParametricPlot[ Evaluate@ Lituus[][t], {t, 0.05, 6 2 Pi}, PlotRange->All, AspectRatio->Automatic]
";

Nephroid::usage =
"Nephroid[][t] return a parametrization for the curve nephroid.
Parameter range: 0 <= t < 2 Pi.
* Nephroid is a special case of epitrochoid (see ?Epitrochoid).
* It is the catacaustic of a circle, with lightsource at infinity.
Example:
ParametricPlot[ Evaluate@ Nephroid[][t], {t, 0, 2 Pi}]
";

Parabola::usage =
"Parabola[][t] return {t, 1/4 t^2}
a parametrization for parabola with focus at {0,1} and vertex at {0,0}.
Cartesian equation is y == 1/4 x^2.
Example:
ParametricPlot[ Evaluate[ Parabola[][t]], {t,-5,5}]
";

ParabolicSpiral::usage =
"Parabolic spiral (aka Fermat's spiral)
is defined by the polar equation r == Sqrt[theta].
It is so called because the similarity to the
parabola equation x == Sqrt[y].
Example:
ParametricPlot[ Evaluate@ ParabolicSpiral[][t], {t, 0, 20 Pi}]
";

Piriform::usage =
"Piriform[h][t] return {1,Sin[t]/h}*(1+Cos[t]) that is the parametric representation of the curve Piriform.
Example:
ParametricPlot[ Evaluate[ Piriform[2][t]], {t,0,2*Pi}]
";

QuadratrixOfHippias::usage =
"QuadratrixOfHippias[][t] return a parametrization for the
curve Quadratrix of Hippias.
Parameter range: -Infinity < t < Infinity, t != n Pi. n is an integer.
Example:
ParametricPlot[ Evaluate@ QuadratrixOfHippias[][t], {t, -3 Pi, 3 Pi}, PlotRange->{{-3 Pi, 3 Pi },{-6,6}}]
";

Rose::usage =
"Rose[n][t] return a parametrization for the curve rose with n petals.
n >= 2. Parameter range: 0 <= t < Pi or 2 Pi.

The polar equation for rose is r == Cos[n theta].
If n is odd, the rose will have n pedals.
If n is even, it will have 2 n pedals.
Rose is a special case of hypotrochoid. See ?Hypotrochoid for detail.
Rose with 3 petals are called trifolium, 4:quadrifolium.
Rose is also known as Rhodonea.
Example:
ParametricPlot[ Evaluate@ Rose[3][t],{t, 0,  Pi}]
";

SemicubicParabola::usage =
"SemicubicParabola[][t] return a parametrization for the curve semi-cubic parabola.
This parametrization is the evolute of the parabola
{(-4 t)/9, -8/27 + t^2/3}.
Cartesian equation is x^2 == y^3.
Example:
ParametricPlot[Evaluate@SemicubicParabola[][t], {t, -3, 3}]
";

Serpentine::usage =
"Serpentine[a,b][t] return a parametrization for the curve Serpentine.
Parameter range: -Infinity < t < Infinity.
Cartesian equation: x^2 y + a^2 y - b^2 x == 0.Example:
ParametricPlot[ Evaluate@ Serpentine[2,4][t], {t, -8,8}]
";

Trisectrix::usage =
"Trisectrix[][t] return a parametrization for the curve Trisectrix.
Parameter range: 0 <= t < 2 Pi.
Cartesian equation is (-2 x + x^2 + y^2)^2 == x^2 + y^2.
Example:
ParametricPlot[ Evaluate@ Trisectrix[][t], {t, 0, 2 Pi}]
";

TrisectrixOfMaclaurin::usage =
"TrisectrixOfMaclaurin[][t] return a parametrization for the curve trisectrix of Maclaurin.
Parameter range: -Infinity < t < Infinity.
Cartesian equation is y^2 (1-x) == x^2 (x + 3). Another parametric
form is (1-4 Cos[t]^2) {1, Tan[t]}, -Pi/2 < t < Pi/2.
Example:
ParametricPlot[ Evaluate@ TrisectrixOfMaclaurin[][t], {t,-5,5} ]
";

Tractrix::usage =
"Tractrix[][t] return a parametrization for the curve tractrix.
Parameter range: -Pi/2 < t < Pi/2.
* Tractrix (aka equitangential curve, tractory) is the involute of catenary.
* Any tangent segment from the tangent point on the curve to the curve's asymptote is constant.
* It is the track made by the rare wheel of a bicycle after a turn.
Example:
ParametricPlot[ Evaluate@ Tractrix[][t], {t,-1.53,1.53}]
";

Trochoid::usage =
"Trochoid[r,h][t] return a parametrization for trochoid.
Parameter range: 0 <= t <= 2 Pi.

Trochoid is defined as the trace of a point P fixed in
position with respect to circle C rolling on a line. r is the radius
of the rolling circle.
h is the distance from the center of the circle to the tracing point.
Special names are given for some settings:

h == r, cycloid, tautochrone, brachistochrone.
h > r, extended (prolate) cycloid.
h < r, contracted (curtate) cycloid.
Example:
ParametricPlot[ Evaluate@ Trochoid[1,2][t], {t, 0, 2*2 Pi}]
";

WitchOfAgnesi::usage =
"WitchOfAgnesi[][t] return a parametrization for the curve Witch of Agnesi (aka versiera).
Parameter range: -Pi/2 < t < Pi/2.
Cartesian equation is 8 x^2 y == 4 (2 - 2 y).
Example:
ParametricPlot[ Evaluate@ WitchOfAgnesi[][t], {t, -1.2, 1.2}]
";

Begin["`Private`"]

ArchimedeanSpiral[n_] := #^n { Cos[#], Sin[#]}&;

Astroid[] = Hypotrochoid[1,1/4,1/4];

BowditchCurve[m_,c_] := {Sin[c + m #], Sin[#]}&;

Cardioid[] = Epitrochoid[1,1,1];

CassinianOval[a_,b_] :=
Sqrt[(2 a^2 Cos[2 #] + Sqrt[-4 (a^4 - b^4) + 4 a^4 Cos[2 #]^2])/2]*
{Cos@#, Sin@#} &

Catenary[] = {#, Cosh[#]}&;

CissoidOfDiocles[] = Sin[#]^2 {1 , Tan[#]}&;

Clothoid[n_] := {Integrate[Sin[x^(n + 1)/(n + 1)], {x, 0, #1}], Integrate[Cos[x^(n + 1)/(n + 1)], {x, 0, #1}]} & ;

ConchoidOfNicomedes[a_,k_] := (a Sec[#] + k) {Sin@ #, Cos@ #}&;

Conics[e_] := e/(1+e Cos@#) {Cos@#, Sin@#}&;

CrossCurve[] = {Sec[#],Csc[#]}&;

Cycloid[] = Trochoid[1,1];

Deltoid[] = Hypotrochoid[1,1/3,1/3];

Ellipse[e_] := {Cos@#, Sqrt[1-e^2] Sin@#}&;
(* e = Sqrt[a^2-b^2]/a *)

Epicycloid[n_] := Epitrochoid[1,1/n,1/n];

Epitrochoid[a_,b_,h_] :=
Function[{t},
 {(a+b) Cos[t] + h Cos[(a+b)/b t], (a+b) Sin[t] + h Sin[(a+b)/b t]}
];

EquiangularSpiral[angle_] := E^(# Cot[angle]) {Cos[#], Sin[#] }&;

FoliumOfDescartes[] = (#^2-1)/(3 #^2+1) {1, #} &;

Hyperbola[e_] := {-Sec[#], Sqrt[e^2-1] Tan[#]}&;
(* a Sqrt[ e^2-1] == b *)
(* e = c/a for all conics *)

HyperbolicSpiral[] = ArchimedeanSpiral[-1];

Hypocycloid[n_] := Hypotrochoid[1,1/n,1/n];

Hypotrochoid[a_,b_,h_] :=
Function[{t},
 {(a-b) Cos[t] + h Cos[-(a-b)/b t], (a-b) Sin[t] + h Sin[-(a-b)/b t]}
];

LemniscateOfBernoulli[] = Cos[#]/(1+Sin[#]^2) {1, Sin[#]}&;

LemniscateOfGerono[] = Cos@# {1, Sin@#}&;

LimaconOfPascal[a_, h_] := (2 a Cos[#] + h){Cos@#, Sin@#}&;

Lituus[] = ArchimedeanSpiral[-1/2];

Nephroid[] = Epitrochoid[1,1/2,1/2];

Parabola[] = {#, 1/4 #^2}&;

ParabolicSpiral[] = ArchimedeanSpiral[1/2];

Piriform[h_] := {1,Sin[#]/h}*(1+Cos[#])&;

QuadratrixOfHippias[] = # {1, Cot[#]}&;

Rose[n_] := Cos[n #] {Cos@#, Sin@#}&;

SemicubicParabola[] = {#^3, #^2}&;

Serpentine[a_,b_] := {#, b^2 #/(a^2+#^2)}&;

Tractrix[] = {Log[ Sec[#] + Tan[#] ] - Sin[#], Cos[#] }&;

Trisectrix[] = LimaconOfPascal[1,1];

TrisectrixOfMaclaurin[] = (#^2-3)/(#^2+1) { 1, # }&;

Trochoid[r_,h_] := {r # - h Sin@#, r - h Cos@#}&;

WitchOfAgnesi[] = { Tan@#, Cos[#]^2}&;

End[]

EndPackage[]
