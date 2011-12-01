
module Graphics.PictureUtil where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry

pictureContains :: Point -> Picture -> Bool
pictureContains (x, y) pict = case pict of
	Line path -> pathContains (x, y) path
	Color _ pic -> pictureContains (x, y) pic
	Translate xt yt pic -> pictureContains (x - xt, y - yt) pic
	Pictures pics -> any (pictureContains (x, y)) pics
	_ -> False

-- | A closed path contains a point if a radial line originating from the point crosses an odd number of boundaries
pathContains pt path = not (null path) && head path == last path &&
	(odd . length . filter id . map (intersectSegRightHorLine pt) . pathSegments) path

pathSegments [] = []
pathSegments [p] = [(p, p)]
pathSegments (p : ps) = snd $ foldl f (p, []) ps
	where f (prevPt, segs) pt = (pt, (prevPt, pt) : segs)

-- | Does seg intersects horizontal line to the right of point
intersectSegRightHorLine (x, y) (p1, p2) = maybe False ((x <) . fst) mp
	where mp = intersectSegHorzLine p1 p2 y