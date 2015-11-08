DECLARE
g1 text;
g2 text;
g3 text;
g4 text;
g5 text;
g6 text;
g7 text;
g8 text;
g text[];
r integer;
tempo text;
elite text;
ttt numeric;
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
UPDATE randtable set 
gene1 = substr(unified,1,2),
gene2 = substr(unified,3,2),
gene3 = substr(unified,5,2),
gene4 = substr(unified,7,2);

-- Elite Choice(2)
select gene into elite from genelist order by fitness desc limit 2;
UPDATE elitetable set elitegene = elite;

-- Crossing
for cr in 1 .. 10
LOOP
crossrate = random();
ttt = random();
if crossrate >= 0.8 then
	select gene1 into g1 from randtable where eid = (2*cr - 1);
	select gene2 into g2 from randtable where eid = (2*cr - 1);
	select gene3 into g3 from randtable where eid = (2*cr - 1);
	select gene4 into g4 from randtable where eid = (2*cr - 1);
	select gene1 into g5 from randtable where eid = 2*cr;
	select gene2 into g6 from randtable where eid = 2*cr;
	select gene3 into g7 from randtable where eid = 2*cr;
	select gene4 into g8 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = (g1 || g2 || g3 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g5 || g6 || g6 || g8) where eid = 2*cr;
else
if ttt <= 0.25 then
	select gene1 into g5 from randtable where eid = (2*cr - 1);
	select gene2 into g6 from randtable where eid = 2*cr;
	select gene3 into g7 from randtable where eid = 2*cr;
	select gene4 into g8 from randtable where eid = 2*cr;
	select gene1 into g1 from randtable where eid = 2*cr;
	select gene2 into g2 from randtable where eid = (2*cr - 1);
	select gene3 into g3 from randtable where eid = (2*cr - 1);
	select gene4 into g4 from randtable where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g1 || g2 || g3 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g5 || g6 || g6 || g8) where eid = 2*cr;
elsif ttt <= 0.5 and ttt >0.25 then
	select gene1 into g5 from randtable where eid = (2*cr - 1);
	select gene2 into g6 from randtable where eid = (2*cr - 1);
	select gene3 into g7 from randtable where eid = 2*cr;
	select gene4 into g8 from randtable where eid = 2*cr;
	select gene1 into g1 from randtable where eid = 2*cr;
	select gene2 into g2 from randtable where eid = 2*cr;
	select gene3 into g3 from randtable where eid = (2*cr - 1);
	select gene4 into g4 from randtable where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g1 || g2 || g3 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g5 || g6 || g6 || g8) where eid = 2*cr;
elsif ttt <= 0.75 and ttt > 0.5 then
	select gene1 into g5 from randtable where eid = (2*cr - 1);
	select gene2 into g6 from randtable where eid = (2*cr - 1);
	select gene3 into g7 from randtable where eid = (2*cr - 1);
	select gene4 into g8 from randtable where eid = 2*cr;
	select gene1 into g1 from randtable where eid = 2*cr;
	select gene2 into g2 from randtable where eid = 2*cr;
	select gene3 into g3 from randtable where eid = 2*cr;
	select gene4 into g4 from randtable where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g1 || g2 || g3 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g5 || g6 || g6 || g8) where eid = 2*cr;
elsif ttt <= 1 and ttt > 0.75 then
	select gene1 into g5 from randtable where eid = (2*cr - 1);
	select gene2 into g6 from randtable where eid = (2*cr - 1);
	select gene3 into g7 from randtable where eid = (2*cr - 1);
	select gene4 into g8 from randtable where eid = (2*cr - 1);
	select gene1 into g1 from randtable where eid = 2*cr;
	select gene2 into g2 from randtable where eid = 2*cr;
	select gene3 into g3 from randtable where eid = 2*cr;
	select gene4 into g4 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = (g1 || g2 || g3 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g5 || g6 || g6 || g8) where eid = 2*cr;
end if;
end if;
end loop;

-- mutation
for mu in 1 .. 20
loop
	mutationrate = random();
	if mutationrate <= 0.05 then
	-- define mutation place
		mupl = round(random() * 3) + 1;
		select newsons into 
		-- Replace new gene at mutation place
		UPDATE randtable set newsons = (select replace(substr()))where eid = mu;
	end if;
end loop;

return new;
end;