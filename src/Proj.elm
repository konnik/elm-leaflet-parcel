module Proj exposing (fromGrid, fromLatLong)

{-| Transformation mellan geodetiska (SWEREF 99) koordinater och grid (SWEREF 99 TM) koordinater.

Formler hämtade från: <https://www.lantmateriet.se/globalassets/kartor-och-geografisk-information/gps-och-geodetisk-matning/gauss_conformal_projection.pdf>

Läs mer här: <https://www.lantmateriet.se/sv/Kartor-och-geografisk-information/gps-geodesi-och-swepos/Referenssystem/Tvadimensionella-system/SWEREF-99-projektioner/>

-}

-- KONSTANTER


{-| Semi major axis of the ellipsoid
-}
a : Float
a =
    6378137.0


{-| Flattening of the ellipsoid
-}
f : Float
f =
    1 / 298.257222101


{-| Longitude of the central meridian
-}
lambda0 : Float
lambda0 =
    degrees 15


{-| Scale factor along the central meridian
-}
k0 : Float
k0 =
    0.9996


{-| False northing
-}
fn : Float
fn =
    0


{-| False easting
-}
fe : Float
fe =
    500000


{-| Transforms a point in grid coordinates (x, y) to geodetic coordinates (lat, long) .

    { lat, long } =
        fromGrid { x, y }

Note: the angles in the result is in expressed in radians

-}
fromGrid : ( Float, Float ) -> { lat : Float, long : Float }
fromGrid ( x, y ) =
    let
        xi =
            (x - fn) / (k0 * aRoof)

        eta =
            (y - fe) / (k0 * aRoof)

        xiPrim =
            xi
                - (d1 * sin (2 * xi) * cosh (2 * eta))
                - (d2 * sin (4 * xi) * cosh (4 * eta))
                - (d3 * sin (6 * xi) * cosh (6 * eta))
                - (d4 * sin (8 * xi) * cosh (8 * eta))

        etaPrim =
            eta
                - (d1 * cos (2 * xi) * sinh (2 * eta))
                - (d2 * cos (4 * xi) * sinh (4 * eta))
                - (d3 * cos (6 * xi) * sinh (6 * eta))
                - (d4 * cos (8 * xi) * sinh (8 * eta))

        d1 =
            (1 / 2) * n - (2 / 3) * n ^ 2 + (37 / 96) * n ^ 3 - (1 / 360) * n ^ 4

        d2 =
            (1 / 48) * n ^ 2 + (1 / 15) * n ^ 3 - (437 / 1440) * n ^ 4

        d3 =
            (17 / 480) * n ^ 3 - (37 / 840) * n ^ 4

        d4 =
            (4397 / 161280) * n ^ 4

        phiStar =
            asin (sin xiPrim / cosh etaPrim)

        dLambda =
            atan2 (sinh etaPrim) (cos xiPrim)

        lambda =
            lambda0 + dLambda

        phi =
            phiStar + sin phiStar * cos phiStar * (aStar + bStar * sin2 phiStar + cStar * sin4 phiStar + dStar * sin6 phiStar)

        aStar =
            e2 + e4 + e6 + e8

        bStar =
            -(1 / 6) * (7 * e4 + 17 * e6 + 30 * e8)

        cStar =
            (1 / 120) * (224 * e6 + 889 * e8)

        dStar =
            -(1 / 1260) * (4279 * e8)
    in
    { lat = phi, long = lambda }


{-| Transforms a point in geodetic coordinates (lat, long) to grid coordinates (x, y).

    { x, y } =
        fromGrid { lat = degrees 69.061939, long = degrees 20.546494 }

Note: the angles representing lat/long should be expressed in radians.

-}
fromLatLong : { lat : Float, long : Float } -> { x : Float, y : Float }
fromLatLong { lat, long } =
    let
        phi =
            lat

        lambda =
            long

        phiStar =
            phi - sin phi * cos phi * (bigA + bigB * sin2 phi + bigC * sin4 phi + bigD * sin6 phi)

        bigA =
            e2

        bigB =
            (1 / 6) * (5 * e4 - e6)

        bigC =
            (1 / 120) * (104 * e6 - 45 * e8)

        bigD =
            (1 / 1260) * (1237 * e8)

        dLambda =
            lambda - lambda0

        xiPrim =
            atan2 (tan phiStar) (cos dLambda)

        etaPrim =
            atanh (cos phiStar * sin dLambda)

        b1 =
            (1 / 2) * n - (2 / 3) * n ^ 2 + (5 / 16) * n ^ 3 + (41 / 180) * n ^ 4

        b2 =
            (13 / 48) * n ^ 2 - (3 / 5) * n ^ 3 + (557 / 1440) * n ^ 4

        b3 =
            (61 / 240) * n ^ 3 - (103 / 140) * n ^ 4

        b4 =
            (49561 / 161280) * n ^ 4

        x =
            k0
                * aRoof
                * (xiPrim
                    + (b1 * sin (2 * xiPrim) * cosh (2 * etaPrim))
                    + (b2 * sin (4 * xiPrim) * cosh (4 * etaPrim))
                    + (b3 * sin (6 * xiPrim) * cosh (6 * etaPrim))
                    + (b4 * sin (8 * xiPrim) * cosh (8 * etaPrim))
                  )
                + fn

        y =
            k0
                * aRoof
                * (etaPrim
                    + (b1 * cos (2 * xiPrim) * sinh (2 * etaPrim))
                    + (b2 * cos (4 * xiPrim) * sinh (4 * etaPrim))
                    + (b3 * cos (6 * xiPrim) * sinh (6 * etaPrim))
                    + (b4 * cos (8 * xiPrim) * sinh (8 * etaPrim))
                  )
                + fe
    in
    { x = x, y = y }



-- Derived constants


e2 : Float
e2 =
    f * (2 - f)


e4 : Float
e4 =
    e2 ^ 2


e6 : Float
e6 =
    e2 ^ 3


e8 : Float
e8 =
    e2 ^ 4


n : Float
n =
    f / (2 - f)


aRoof : Float
aRoof =
    (a / (1 + n)) * (1 + (1 / 4) * n ^ 2 + (1 / 64) * n ^ 4)



-- Define some utility functions


sin2 : Float -> Float
sin2 x =
    sin x ^ 2


sin4 : Float -> Float
sin4 x =
    sin x ^ 4


sin6 : Float -> Float
sin6 x =
    sin x ^ 6



-- Definiera hyperboliska trigonometrifunktioner som saknas i Elm.
-- https://github.com/elm/core/issues/968


cosh : Float -> Float
cosh x =
    (e ^ x + e ^ -x) / 2


sinh : Float -> Float
sinh x =
    (e ^ x - e ^ -x) / 2


atanh : Float -> Float
atanh x =
    logBase e ((1 + x) / (1 - x)) / 2
