{-# LANGUAGE BangPatterns #-}

{-
This solution is terrible. It takes minutes to run and uses several GBs of memory.
Probably should've used a mutable array instead, or at least used a smarter algorithm.
-}

import qualified Data.Array as A
    
type Coords = (Int, Int)
type Lights = A.Array Coords Bool

type DimmableLights = A.Array Coords Int

data Instruction = TurnOn | TurnOff | Toggle deriving (Show)
type InstructionList = [(Instruction, Coords, Coords)]

input :: InstructionList
input = [(TurnOn, (489,959), (759,964)),
    (TurnOff, (820,516), (871,914)),
    (TurnOff, (427,423), (929,502)),
    (TurnOn, (774,14), (977,877)),
    (TurnOn, (410,146), (864,337)),
    (TurnOn, (931,331), (939,812)),
    (TurnOff, (756,53), (923,339)),
    (TurnOff, (313,787), (545,979)),
    (TurnOff, (12,823), (102,934)),
    (Toggle, (756,965), (812,992)),
    (TurnOff, (743,684), (789,958)),
    (Toggle, (120,314), (745,489)),
    (Toggle, (692,845), (866,994)),
    (TurnOff, (587,176), (850,273)),
    (TurnOff, (674,321), (793,388)),
    (Toggle, (749,672), (973,965)),
    (TurnOn, (943,30), (990,907)),
    (TurnOn, (296,50), (729,664)),
    (TurnOn, (212,957), (490,987)),
    (Toggle, (171,31), (688,88)),
    (TurnOff, (991,989), (994,998)),
    (TurnOff, (913,943), (958,953)),
    (TurnOff, (278,258), (367,386)),
    (Toggle, (275,796), (493,971)),
    (TurnOff, (70,873), (798,923)),
    (Toggle, (258,985), (663,998)),
    (TurnOn, (601,259), (831,486)),
    (TurnOff, (914,94), (941,102)),
    (TurnOff, (558,161), (994,647)),
    (TurnOn, (119,662), (760,838)),
    (Toggle, (378,775), (526,852)),
    (TurnOff, (384,670), (674,972)),
    (TurnOff, (249,41), (270,936)),
    (TurnOn, (614,742), (769,780)),
    (TurnOn, (427,70), (575,441)),
    (TurnOn, (410,478), (985,753)),
    (TurnOff, (619,46), (931,342)),
    (TurnOn, (284,55), (768,922)),
    (TurnOff, (40,592), (728,685)),
    (TurnOn, (825,291), (956,950)),
    (TurnOn, (147,843), (592,909)),
    (TurnOff, (218,675), (972,911)),
    (Toggle, (249,291), (350,960)),
    (TurnOff, (556,80), (967,675)),
    (Toggle, (609,148), (968,279)),
    (Toggle, (217,605), (961,862)),
    (Toggle, (407,177), (548,910)),
    (Toggle, (400,936), (599,938)),
    (TurnOff, (721,101), (925,455)),
    (TurnOn, (268,631), (735,814)),
    (Toggle, (549,969), (612,991)),
    (Toggle, (553,268), (689,432)),
    (TurnOff, (817,668), (889,897)),
    (Toggle, (801,544), (858,556)),
    (Toggle, (615,729), (832,951)),
    (TurnOff, (427,477), (958,948)),
    (TurnOn, (164,49), (852,946)),
    (TurnOn, (542,449), (774,776)),
    (TurnOff, (923,196), (980,446)),
    (Toggle, (90,310), (718,846)),
    (TurnOff, (657,215), (744,252)),
    (TurnOff, (800,239), (811,712)),
    (TurnOn, (502,90), (619,760)),
    (Toggle, (649,512), (862,844)),
    (TurnOff, (334,903), (823,935)),
    (TurnOff, (630,233), (839,445)),
    (TurnOn, (713,67), (839,865)),
    (TurnOn, (932,50), (982,411)),
    (TurnOff, (480,729), (984,910)),
    (TurnOn, (100,219), (796,395)),
    (TurnOn, (758,108), (850,950)),
    (TurnOff, (427,276), (439,938)),
    (TurnOn, (178,284), (670,536)),
    (Toggle, (540,27), (625,102)),
    (TurnOff, (906,722), (936,948)),
    (Toggle, (345,418), (859,627)),
    (Toggle, (175,775), (580,781)),
    (Toggle, (863,28), (929,735)),
    (TurnOff, (824,858), (905,973)),
    (Toggle, (752,312), (863,425)),
    (TurnOn, (985,716), (988,852)),
    (TurnOff, (68,504), (763,745)),
    (Toggle, (76,209), (810,720)),
    (TurnOff, (657,607), (676,664)),
    (Toggle, (596,869), (896,921)),
    (TurnOff, (915,411), (968,945)),
    (TurnOff, (368,39), (902,986)),
    (TurnOn, (11,549), (393,597)),
    (TurnOff, (842,893), (976,911)),
    (Toggle, (274,106), (581,329)),
    (Toggle, (406,403), (780,950)),
    (Toggle, (408,988), (500,994)),
    (Toggle, (217,73), (826,951)),
    (TurnOn, (917,872), (961,911)),
    (Toggle, (394,34), (510,572)),
    (Toggle, (424,603), (583,626)),
    (Toggle, (106,159), (755,738)),
    (TurnOff, (244,610), (472,709)),
    (TurnOn, (350,265), (884,690)),
    (TurnOn, (688,184), (928,280)),
    (Toggle, (279,443), (720,797)),
    (TurnOff, (615,493), (888,610)),
    (Toggle, (118,413), (736,632)),
    (TurnOn, (798,782), (829,813)),
    (TurnOff, (250,934), (442,972)),
    (TurnOn, (68,503), (400,949)),
    (Toggle, (297,482), (313,871)),
    (Toggle, (710,3), (839,859)),
    (TurnOn, (125,300), (546,888)),
    (Toggle, (482,39), (584,159)),
    (TurnOff, (536,89), (765,962)),
    (TurnOn, (530,518), (843,676)),
    (TurnOn, (994,467), (994,676)),
    (TurnOn, (623,628), (744,927)),
    (Toggle, (704,912), (837,983)),
    (TurnOn, (154,364), (517,412)),
    (Toggle, (344,409), (780,524)),
    (TurnOff, (578,740), (725,879)),
    (TurnOn, (251,933), (632,957)),
    (TurnOn, (827,705), (971,789)),
    (Toggle, (191,282), (470,929)),
    (Toggle, (324,525), (446,867)),
    (Toggle, (534,343), (874,971)),
    (Toggle, (550,650), (633,980)),
    (Toggle, (837,404), (881,915)),
    (Toggle, (338,881), (845,905)),
    (TurnOn, (469,462), (750,696)),
    (TurnOn, (741,703), (892,870)),
    (TurnOff, (570,215), (733,562)),
    (TurnOn, (445,576), (870,775)),
    (TurnOn, (466,747), (554,878)),
    (TurnOff, (820,453), (868,712)),
    (TurnOff, (892,706), (938,792)),
    (TurnOff, (300,238), (894,746)),
    (TurnOff, (306,44), (457,444)),
    (TurnOff, (912,569), (967,963)),
    (Toggle, (109,756), (297,867)),
    (TurnOn, (37,546), (41,951)),
    (TurnOn, (321,637), (790,910)),
    (Toggle, (66,50), (579,301)),
    (Toggle, (933,221), (933,791)),
    (TurnOn, (486,676), (878,797)),
    (TurnOn, (417,231), (556,317)),
    (Toggle, (904,468), (981,873)),
    (TurnOn, (417,675), (749,712)),
    (TurnOn, (692,371), (821,842)),
    (Toggle, (324,73), (830,543)),
    (TurnOn, (912,490), (977,757)),
    (TurnOff, (634,872), (902,949)),
    (Toggle, (266,779), (870,798)),
    (TurnOn, (772,982), (990,996)),
    (TurnOff, (607,46), (798,559)),
    (TurnOn, (295,602), (963,987)),
    (TurnOn, (657,86), (944,742)),
    (TurnOff, (334,639), (456,821)),
    (TurnOff, (997,667), (997,670)),
    (TurnOff, (725,832), (951,945)),
    (TurnOff, (30,120), (952,984)),
    (TurnOn, (860,965), (917,976)),
    (Toggle, (471,997), (840,998)),
    (TurnOff, (319,307), (928,504)),
    (Toggle, (823,631), (940,908)),
    (Toggle, (969,984), (981,993)),
    (TurnOff, (691,319), (865,954)),
    (Toggle, (911,926), (938,929)),
    (TurnOn, (953,937), (968,991)),
    (Toggle, (914,643), (975,840)),
    (TurnOn, (266,982), (436,996)),
    (TurnOff, (101,896), (321,932)),
    (TurnOff, (193,852), (751,885)),
    (TurnOff, (576,532), (863,684)),
    (TurnOn, (761,456), (940,783)),
    (TurnOn, (20,290), (398,933)),
    (TurnOff, (435,335), (644,652)),
    (TurnOn, (830,569), (905,770)),
    (TurnOff, (630,517), (905,654)),
    (TurnOn, (664,53), (886,976)),
    (Toggle, (275,416), (408,719)),
    (TurnOn, (370,621), (515,793)),
    (TurnOn, (483,373), (654,749)),
    (TurnOn, (656,786), (847,928)),
    (TurnOff, (532,752), (945,974)),
    (Toggle, (301,150), (880,792)),
    (TurnOff, (951,488), (958,952)),
    (TurnOn, (207,729), (882,828)),
    (Toggle, (694,532), (973,961)),
    (Toggle, (676,639), (891,802)),
    (TurnOff, (653,6), (905,519)),
    (Toggle, (391,109), (418,312)),
    (TurnOn, (877,423), (957,932)),
    (TurnOn, (340,145), (563,522)),
    (TurnOff, (978,467), (988,895)),
    (TurnOff, (396,418), (420,885)),
    (TurnOff, (31,308), (816,316)),
    (TurnOn, (107,675), (758,824)),
    (TurnOn, (61,82), (789,876)),
    (TurnOn, (750,743), (754,760)),
    (Toggle, (88,733), (736,968)),
    (TurnOff, (754,349), (849,897)),
    (Toggle, (157,50), (975,781)),
    (TurnOff, (230,231), (865,842)),
    (TurnOff, (516,317), (630,329)),
    (TurnOff, (697,820), (829,903)),
    (TurnOn, (218,250), (271,732)),
    (Toggle, (56,167), (404,431)),
    (Toggle, (626,891), (680,927)),
    (Toggle, (370,207), (791,514)),
    (Toggle, (860,74), (949,888)),
    (TurnOn, (416,527), (616,541)),
    (TurnOff, (745,449), (786,908)),
    (TurnOn, (485,554), (689,689)),
    (TurnOn, (586,62), (693,141)),
    (Toggle, (506,759), (768,829)),
    (TurnOn, (473,109), (929,166)),
    (TurnOn, (760,617), (773,789)),
    (Toggle, (595,683), (618,789)),
    (TurnOff, (210,775), (825,972)),
    (Toggle, (12,426), (179,982)),
    (TurnOn, (774,539), (778,786)),
    (TurnOn, (102,498), (121,807)),
    (TurnOff, (706,897), (834,965)),
    (TurnOff, (678,529), (824,627)),
    (TurnOn, (7,765), (615,870)),
    (TurnOff, (730,872), (974,943)),
    (TurnOff, (595,626), (836,711)),
    (TurnOff, (215,424), (841,959)),
    (Toggle, (341,780), (861,813)),
    (Toggle, (507,503), (568,822)),
    (TurnOn, (252,603), (349,655)),
    (Toggle, (93,521), (154,834)),
    (TurnOn, (565,682), (951,954)),
    (TurnOn, (544,318), (703,418)),
    (Toggle, (756,953), (891,964)),
    (TurnOn, (531,123), (856,991)),
    (TurnOn, (148,315), (776,559)),
    (TurnOff, (925,835), (963,971)),
    (TurnOn, (895,944), (967,964)),
    (TurnOff, (102,527), (650,747)),
    (Toggle, (626,105), (738,720)),
    (TurnOff, (160,75), (384,922)),
    (Toggle, (813,724), (903,941)),
    (TurnOn, (207,107), (982,849)),
    (Toggle, (750,505), (961,697)),
    (Toggle, (105,410), (885,819)),
    (TurnOn, (226,104), (298,283)),
    (TurnOff, (224,604), (508,762)),
    (TurnOn, (477,368), (523,506)),
    (TurnOff, (477,901), (627,936)),
    (TurnOff, (887,131), (889,670)),
    (TurnOn, (896,994), (938,999)),
    (Toggle, (401,580), (493,728)),
    (Toggle, (987,184), (991,205)),
    (TurnOn, (821,643), (882,674)),
    (Toggle, (784,940), (968,959)),
    (TurnOff, (251,293), (274,632)),
    (TurnOff, (339,840), (341,844)),
    (TurnOff, (675,351), (675,836)),
    (Toggle, (918,857), (944,886)),
    (Toggle, (70,253), (918,736)),
    (TurnOff, (612,604), (772,680)),
    (TurnOff, (277,40), (828,348)),
    (Toggle, (692,139), (698,880)),
    (Toggle, (124,446), (883,453)),
    (Toggle, (969,932), (990,945)),
    (Toggle, (855,692), (993,693)),
    (Toggle, (722,472), (887,899)),
    (Toggle, (978,149), (985,442)),
    (Toggle, (837,540), (916,889)),
    (TurnOff, (612,2), (835,82)),
    (Toggle, (560,767), (878,856)),
    (TurnOn, (461,734), (524,991)),
    (Toggle, (206,824), (976,912)),
    (TurnOn, (826,610), (879,892)),
    (TurnOn, (577,699), (956,933)),
    (TurnOff, (9,250), (50,529)),
    (TurnOff, (77,657), (817,677)),
    (TurnOn, (68,419), (86,426)),
    (TurnOn, (991,720), (992,784)),
    (TurnOn, (668,20), (935,470)),
    (TurnOff, (133,418), (613,458)),
    (TurnOff, (487,286), (540,328)),
    (Toggle, (247,874), (840,955)),
    (Toggle, (301,808), (754,970)),
    (TurnOff, (34,194), (578,203)),
    (TurnOff, (451,49), (492,921)),
    (TurnOn, (907,256), (912,737)),
    (TurnOff, (479,305), (702,587)),
    (TurnOn, (545,583), (732,749)),
    (Toggle, (11,16), (725,868)),
    (TurnOn, (965,343), (986,908)),
    (TurnOn, (674,953), (820,965)),
    (Toggle, (398,147), (504,583)),
    (TurnOff, (778,194), (898,298)),
    (TurnOn, (179,140), (350,852)),
    (TurnOff, (241,118), (530,832)),
    (TurnOff, (41,447), (932,737)),
    (TurnOff, (820,663), (832,982)),
    (TurnOn, (550,460), (964,782)),
    (TurnOn, (31,760), (655,892)),
    (Toggle, (628,958), (811,992))]
                 
createLights :: Coords -> Lights
createLights (x, y) = A.array ((0, 0), (x, y)) [((x_, y_), False) | x_ <- [0..x], y_ <- [0..y]]

createDimmableLights :: Coords -> DimmableLights
createDimmableLights (x, y) = A.array ((0, 0), (x, y)) [((x_, y_), 0) | x_ <- [0..x], y_ <- [0..y]]

lightChanges :: (a -> a) -> [(Coords, a)] -> Coords -> Coords -> [(Coords, a)]
lightChanges f !lights from to =
    map (\(i, e) -> (i, f e)) (filter (\(i, _) -> isInRange i from to) lights)

isInRange :: Coords -> Coords -> Coords -> Bool
isInRange (x, y) (xf, yf) (xt, yt) = x >= xf && x <= xt && y >= yf && y <= yt

changeLights :: (a -> a) -> A.Array Coords a -> Coords -> Coords -> IO (A.Array Coords a)
changeLights f !lights from to = do
  let changes = lightChanges f (A.assocs lights) from to
  return $ (A.//) lights changes

turnOn :: Lights -> Coords -> Coords -> IO Lights
turnOn = changeLights (const True)
         
turnOff :: Lights -> Coords -> Coords -> IO Lights
turnOff = changeLights (const False)
         
toggle :: Lights -> Coords -> Coords -> IO Lights
toggle = changeLights not

turnOnDimmable :: DimmableLights -> Coords -> Coords -> IO DimmableLights
turnOnDimmable = changeLights (+1)
         
turnOffDimmable :: DimmableLights -> Coords -> Coords -> IO DimmableLights
turnOffDimmable = changeLights (\n -> if n == 0 then 0 else n-1)
         
toggleDimmable :: DimmableLights -> Coords -> Coords -> IO DimmableLights
toggleDimmable = changeLights (+2)

countLit :: Lights -> Int
countLit = length . filter id . A.elems

sumLights :: DimmableLights -> Int
sumLights = sum . A.elems

runInstruction :: Instruction -> Lights -> Coords -> Coords -> IO Lights
runInstruction TurnOn = turnOn
runInstruction TurnOff = turnOff
runInstruction Toggle = toggle

runDimmableInstruction :: Instruction -> DimmableLights -> Coords -> Coords -> IO DimmableLights
runDimmableInstruction TurnOn = turnOnDimmable
runDimmableInstruction TurnOff = turnOffDimmable
runDimmableInstruction Toggle = toggleDimmable

runInstructions :: Lights -> InstructionList -> IO Lights
runInstructions lights [] = return lights
runInstructions lights ((i, from, to):is) = do
  next <- runInstruction i lights from to
  putStrLn $ "Instruction: " ++ (show (i, from, to))
  runInstructions next is
                 
runDimmableInstructions :: DimmableLights -> InstructionList -> IO DimmableLights
runDimmableInstructions lights [] = return lights
runDimmableInstructions lights ((i, from, to):is) = do
  next <- runDimmableInstruction i lights from to
  putStrLn $ "Instruction: " ++ (show (i, from, to))
  runDimmableInstructions next is

main :: IO ()
main = do
  result1 <- runInstructions (createLights (999,999)) input
  print (countLit result1)

  result2 <- runDimmableInstructions (createDimmableLights (999,999)) input
  print (sumLights result2)
