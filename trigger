DECLARE
r integer;
tempo text;
elite text;
sumfitness numeric;	

BEGIN
-- Probability Calculation and Cumulation
select sum(fitness) into sumfitness from genelist;
UPDATE genelist set probability = fitness / sumfitness;
UPDATE genelist set cumlativeprobab = x.probab from
 (select geneid,(select sum(probability) from genelist where geneid <= x.geneid)
 as probab from genelist as x order by geneid) as x  where genelist.geneid = x.geneid;
-- Roulette Choice(48)
for r in 1 .. 5
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

return new;
end;