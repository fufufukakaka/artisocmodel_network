DECLARE
g1 text;
g2 text;
g3 text;
g4 text;
r integer;
i integer;
tempo text;
elite text;
ttt integer;
beginpoint integer;
endpoint integer;
cr integer;
mu integer;
mupl integer;
crossrate numeric;
mutationrate numeric;
sumfitness numeric;	

BEGIN
-- Probability Calculation and Cumulation
select sum(fitness) into sumfitness from genelist;
UPDATE genelist set probability = fitness / sumfitness;
UPDATE genelist set cumlativeprobab = x.probab from
 (select geneid,(select sum(probability) from genelist where geneid <= x.geneid)
 as probab from genelist as x order by geneid) as x  where genelist.geneid = x.geneid;
-- Roulette Choice(48)
for r in 1 .. 20
LOOP	
	select gene into tempo from genelist where cumlativeprobab <= random() order by cumlativeprobab desc limit 1;
	UPDATE randtable set unified = tempo where eid = r;
end loop;
-- Elite Choice(2)
select gene into elite from genelist order by fitness desc limit 2;
UPDATE elitetable set elitegene = elite;

-- Crossing
for cr in 1 .. 10
LOOP
crossrate = random();
ttt = round(random()*9 + 1);
beginpoint = 2 * ttt - 2;
endpoint = 2 * ttt - 1;
if crossrate >= 0.8 then
-- 交叉率以上ならば、交叉せずそのまま結合する
	select unified into g1 from randtable where eid = (2*cr-1);
	select unified into g3 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = g1 where eid = (2*cr - 1);
	UPDATE randtable set newsons = g3 where eid = 2*cr;
else
-- tttの位置に合わせて交差す
if ttt >= 2 and ttt <= 9 then
	select substr(unified,1,beginpoint) into g1 from randtable where eid = (2*cr - 1);
	select substr(unified,endpoint,(20 - endpoint + 1)) from randtable into g2 where eid = (2*cr - 1);
	select substr(unified,1,beginpoint) from randtable into g3 where eid = 2*cr;
	select substr(unified,endpoint,(20 - endpoint + 1)) from randtable into g4 where eid = 2*cr;
	UPDATE randtable set newsons = (g1 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g3 || g2) where eid = 2*cr;
elsif ttt = 1 then
	select unified into g2 from randtable where eid = (2*cr - 1);
	select unified into g4 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = g4 where eid = (2*cr - 1);
	UPDATE randtable set newsons = g2 where eid = 2*cr;
elsif ttt = 10 then
	select unified into g1 from randtable where eid = (2*cr - 1);
	select unified into g3 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = g1 where eid = (2*cr - 1);
	UPDATE randtable set newsons = g3 where eid = 2*cr;
end if;
end if; -- 交叉終了
end loop; -- 全個体に関する交叉終了

-- mutation
return new;
end;