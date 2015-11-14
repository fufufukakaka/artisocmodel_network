DECLARE
g1 text;
g2 text;
g3 text;
g4 text;
gm1 text;
gm2 text;
genecheck integer;
rand1 integer;
rand2 integer;
gmu1 text;
gmu2 text;
characters text;
randgene text;
r integer;
i integer;
ttt integer;
beginpoint integer;
mubegin integer;
endpoint integer;
muend integer;
cr integer;
mu integer;
mupl integer;
crossrate numeric;
mutationrate numeric;
sumfitness numeric;
minfitness numeric;
avgfitness numeric;
maxfitness numeric;

BEGIN
-- estimatedgeneにinsertされたオブジェクトを取得する
-- 取得したオブジェクトを対応するeidでgeneboardにUPDATEする
UPDATE geneboard set gene = eval.gene,fitness = eval.fitness,
oldgeneration = eval.oldgeneration  from 
(select eid,gene,fitness,oldgeneration from estimatedgene where estimatedgene.eid = new.eid) as eval 
where geneboard.eid = eval.eid;
-- 重複していないならgenelistにgene,generation,fitnessをinsertする
select count(gene) into genecheck from genelist 
where genelist.gene = (select gene from estimatedgene where estimatedgene.eid = new.eid);
if genecheck = 0 then
INSERT into genelist(gene,generation,fitness) 
select new.gene,new.oldgeneration,new.fitness from estimatedgene where estimatedgene.eid = new.eid;
end if;

-- 適応度をスケーリングする
select max(fitness) into maxfitness from geneboard;
select min(fitness) into minfitness from geneboard;
select avg(fitness) into avgfitness from geneboard;
UPDATE geneboard set scalefitness = 
(avgfitness/(avgfitness - minfitness))*fitness - ((minfitness * avgfitness)/(avgfitness - minfitness)); 
UPDATE geneboard set scalefitness = 0 where scalefitness < 0;

-- Probability Calculation and Cumulation
UPDATE geneboard set newgeneration = oldgeneration + 1;
select sum(scalefitness) into sumfitness from geneboard;
UPDATE geneboard set probability = scalefitness / sumfitness;
UPDATE geneboard set cumlativeprobab = x.probab from
 (select eid,(select sum(probability) from geneboard where eid <= x.eid)
 as probab from geneboard as x order by eid) as x  where geneboard.eid = x.eid;
-- Roulette Choice(28)
for r in 1 .. 28
LOOP	
	UPDATE randtable set unified = tempo.gene,newgeneration =  tempo.newgeneration 
	from (select gene,newgeneration from geneboard 
	where cumlativeprobab <= random() order by cumlativeprobab desc limit 1) as tempo
	where randtable.eid = r;
end loop;
-- Elite Choice(2)
UPDATE elitetable set elite = x.gene,newgeneration = x.newg from
(select newgeneration as newg ,gene from geneboard where fitness < (select max(fitness) from geneboard) limit 1)
as x where elitetable.eid = 29;
UPDATE elitetable set elite = x.gene,newgeneration = x.newg from
(select newgeneration as newg ,gene from geneboard where fitness = (select max(fitness) from geneboard))
as x where elitetable.eid = 30;

-- Crossing
for cr in 1 .. 14
LOOP
crossrate = random();
ttt = round(random()*49) + 1;
beginpoint = 2 * ttt - 2;
endpoint = 2 * ttt - 1;
if crossrate >= 0.8 then
-- 交叉率以上ならば、交叉せずそのまま結合する
	select unified into g1 from randtable where eid = (2*cr-1);
	select unified into g3 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = g1 where eid = (2*cr - 1);
	UPDATE randtable set newsons = g3 where eid = 2*cr;
else
-- tttの位置に合わせて交差する
if ttt >= 2 and ttt <= 49 then
	select substr(unified,1,beginpoint) into g1 from randtable where eid = (2*cr - 1);
	select substr(unified,endpoint,(100 - endpoint + 1)) from randtable into g2 where eid = (2*cr - 1);
	select substr(unified,1,beginpoint) from randtable into g3 where eid = 2*cr;
	select substr(unified,endpoint,(100 - endpoint + 1)) from randtable into g4 where eid = 2*cr;
	UPDATE randtable set newsons = (g1 || g4) where eid = (2*cr - 1);
	UPDATE randtable set newsons = (g3 || g2) where eid = 2*cr;
elsif ttt = 1 then
	select unified into g2 from randtable where eid = (2*cr - 1);
	select unified into g4 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = g4 where eid = (2*cr - 1);
	UPDATE randtable set newsons = g2 where eid = 2*cr;
elsif ttt = 50 then
	select unified into g1 from randtable where eid = (2*cr - 1);
	select unified into g3 from randtable where eid = 2*cr;
	UPDATE randtable set newsons = g1 where eid = (2*cr - 1);
	UPDATE randtable set newsons = g3 where eid = 2*cr;
end if;
end if; -- 交叉終了
end loop; -- 全個体に関する交叉終了

-- mutation
-- 交叉と考え方は同じで、突然変異が起こる場所が決まったら遺伝子を3つ(突然変異以前、突然変異、突然変異以後)に分解して、突然変異部分を生成し、くっつける
for mu in 1 .. 28
loop
-- 突然変異するならば次の遺伝子を挿入する
rand1 = round(random()*19)+1;
rand2 = round(random()*19)+1;
characters = 'abcdefghijklmnopqrstu';
gmu1 = substr(characters,rand1,1);
gmu2 = substr(characters,rand2,1);
randgene = (gmu1 || gmu2);
mutationrate = random();
-- define mutation place
mupl = round(random() * 48) + 1;
mubegin = 2*(mupl - 1);
muend = (2*mupl + 1);
-- テストのため、0.8に設定して挙動を見る
	if mutationrate >= 0.05 then
	   -- 突然変異率以上ならば、そのまま結合する
	   select newsons into gm1 from randtable where eid = mu;
	   UPDATE randtable set newsonsmutation = gm1 where eid = mu;
    else
    -- 突然変異率を満たすならば、該当箇所に応じた変異処理をする
        if mupl >= 2 and mupl <= 49 then
            select substr(newsons,1,mubegin) into gm1 from randtable where eid = mu;
            select substr(newsons,muend,(100 - muend + 1)) into gm2 from randtable where eid = mu;
            UPDATE randtable set newsonsmutation = (gm1 || randgene || gm2) where eid = mu;
        elsif mupl = 1 then
            select substr(newsons,3,98) into gm2 from randtable where eid = mu;
            UPDATE randtable set newsonsmutation = (randgene || gm2) where eid = mu;
        elsif mupl = 50 then
            select substr(newsons,1,98) into gm1 from randtable where eid = mu;
            UPDATE randtable set newsonsmutation = (gm1 || randgene) where eid = mu;
        end if;
	end if;
end loop;

-- requestgeneのeidに対応するところにgene,generationをUPDATEする
UPDATE requestgene set gene = roulettelist.gene,newgeneration = roulettelist.newgeneration from
(select eid,newsonsmutation as gene,newgeneration from randtable) as roulettelist 
where requestgene.eid = roulettelist.eid;
UPDATE requestgene set gene = elitelist.elite,newgeneration = elitelist.newgeneration from
(select eid,elite,newgeneration from elitetable) as elitelist 
where requestgene.eid = elitelist.eid;

return new;
end;