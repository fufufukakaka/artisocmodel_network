DECLARE
g1 text;
g2 text;
g3 text;
g4 text;
gm1 text;
gm2 text;
rand1 integer;
rand2 integer;
gmu1 text;
gmu2 text;
characters text;
randgene text;
r integer;
i integer;
tempo text;
elite text;
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


BEGIN
-- Probability Calculation and Cumulation
select sum(fitness) into sumfitness from newgeneration;
UPDATE newgeneration set probability = fitness / sumfitness;
UPDATE newgeneration set cumlativeprobab = x.probab from
 (select geneid,(select sum(probability) from newgeneration where geneid <= x.geneid)
 as probab from newgeneration as x order by geneid) as x  where newgeneration.geneid = x.geneid;
-- Roulette Choice(48)
for r in 1 .. 20
LOOP	
	select gene into tempo from newgeneration where cumlativeprobab <= random() order by cumlativeprobab desc limit 1;
	UPDATE randtable set unified = tempo where eid = r;
end loop;
-- Elite Choice(2)
select gene into elite from newgeneration order by fitness desc limit 2;
UPDATE elitetable set elitegene = elite;

-- Crossing
for cr in 1 .. 10
LOOP
crossrate = random();
ttt = round(random()*9) + 1;
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
-- 交叉と考え方は同じで、突然変異が起こる場所が決まったら遺伝子を3つ(突然変異以前、突然変異、突然変異以後)に分解して、突然変異部分を生成し、くっつける
for mu in 1 .. 20
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
mupl = round(random() * 9) + 1;
mubegin = 2*(mupl - 1);
muend = (2*mupl + 1);
-- テストのため、0.8に設定して挙動を見る
	if mutationrate >= 0.05 then
	   -- 突然変異率以上ならば、そのまま結合する
	   select newsons into gm1 from randtable where eid = mu;
	   UPDATE randtable set newsonsmutation = gm1 where eid = mu;
    else
    -- 突然変異率を満たすならば、該当箇所に応じた変異処理をする
        if mupl >= 2 and mupl <= 9 then
            select substr(newsons,1,mubegin) into gm1 from randtable where eid = mu;
            select substr(newsons,muend,(20 - muend + 1)) into gm2 from randtable where eid = mu;
            UPDATE randtable set newsonsmutation = (gm1 || randgene || gm2) where eid = mu;
        elsif mupl = 1 then
            select substr(newsons,3,18) into gm2 from randtable where eid = mu;
            UPDATE randtable set newsonsmutation = (randgene || gm2) where eid = mu;
        elsif mupl = 10 then
            select substr(newsons,1,18) into gm1 from randtable where eid = mu;
            UPDATE randtable set newsonsmutation = (gm1 || randgene) where eid = mu;
        end if;
	end if;
end loop;

return new;
end;