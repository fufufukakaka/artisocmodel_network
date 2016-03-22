// ルートに関してごにょごにょする
function routegonyogonyo(selectsituation As Integer,listnumber as integer) as string
{
    dim i as integer
    dim k as integer
    dim route(6,11) as string
    dim cost(6,11) as double
    dim probab(6,11) as double
    dim sumprobab as double
    dim ruiseki(6,11) as double
    dim roulettebari as double
    dim rouletteruiseki(6,2) as string
    dim situation as string
    dim eb as string
    dim ebn as integer
    dim tmproute as string
	dim kk as integer

    // ebはenable bridgeだよ
    // ebnはenable bridge numberだよ
    
    // 使える橋リストを初期化
    eb = "s"
    
    // まず全目的地(listnumber)、全橋(usebridge)に関して確率を計算する。
    // そのあとで、入力されたlistnumberに応じたルートをmy.routearrayに代入する
    
    // あらかじめ現在使うことができる橋を調べる
    situation = universe.situation
    for k = 0 to 10
        kk = k + 1
        if mid(situation,kk,1) == "1" then
            eb = eb & "," & k
        end if
    next k
    
    // ebの一文字目を削除
    eb = replace(eb,"s,","")
    
    // ルーレットで選べる橋の数を配列に反映する
    ebn = counttoken(eb)
    
	if selectsituation == 0 then
        // jobroute
        for i = 0 to 5
            for k = 0 to 10
                route(i,k) = my.routecollectionjob(i,k,0)
                cost(i,k) = Cdbl(my.routecollectionjob(i,k,1))
                probab(i,k) = Cdbl(my.routecollectionjob(i,k,2))
            next k
        next i
    elseif selectsituation == 1 then
        // shoproute
        for i = 0 to 5
            for k = 0 to 10
                route(i,k) = my.routecollectionshop(i,k,0)
                cost(i,k) = Cdbl(my.routecollectionshop(i,k,1))
                probab(i,k) = Cdbl(my.routecollectionshop(i,k,2))
            next k
        next i
    elseif selectsituation == 2 then
        // jobhomeroute
        for i = 0 to 5
            for k = 0 to 10
                route(i,k) = my.routecollectionjobhome(i,k,0)
                cost(i,k) = Cdbl(my.routecollectionjobhome(i,k,1))
                probab(i,k) = Cdbl(my.routecollectionjobhome(i,k,2))
            next k
        next i
    elseif selectsituation == 3 then
        // shophomeroute
        for i = 0 to 5
            for k = 0 to 10
                route(i,k) = my.routecollectionshophome(i,k,0)
                cost(i,k) = Cdbl(my.routecollectionshophome(i,k,1))
                probab(i,k) = Cdbl(my.routecollectionshophome(i,k,2))
            next k
        next i
    end if
    
    // 確率を初期化
    for i = 0 to 5
        for k = 0 to 10
            probab(i,k) = 0
            ruiseki(i,k) = 0
        next k
    next i
    
    // コストを用いてルートを選ぶための確率を計算する
    for i = 0 to 5
        sumprobab = 0
        for k = 0 to (ebn - 1)
sumprobab = sumprobab + exp(1 / cost(i,Cint(gettoken(eb,k))))
        next k
        for k = 0 to (ebn - 1)
probab(i,Cint(gettoken(eb,k))) = (exp(1 / cost(i,Cint(gettoken(eb,k))))/sumprobab)
        next k
        for k = 0 to (ebn - 1)
            if k == 0 then
                rouletteruiseki(i,0) = Cstr(probab(i,Cint(gettoken(eb,k))))
            else
                rouletteruiseki(i,0) = rouletteruiseki(i,0) & "," & Cstr(Cdbl(gettoken(rouletteruiseki(i,0),k-1)) + probab(i,Cint(gettoken(eb,k))))
            end if
        next k
        rouletteruiseki(i,1) = eb
    next i

    // 取り敢えず確率を返しておく
    if selectsituation == 0 then
        // jobroute
        for i = 0 to 5
            for k = 0 to 10
                my.routecollectionjob(i,k,2) = Cstr(probab(i,k))
            next k
        next i
    elseif selectsituation == 1 then
        // shoproute
        for i = 0 to 5
            for k = 0 to 10
                my.routecollectionshop(i,k,2) = Cstr(probab(i,k))
            next k
        next i
    elseif selectsituation == 2 then
        // jobhomeroute
        for i = 0 to 5
            for k = 0 to 10
                my.routecollectionjobhome(i,k,2) = Cstr(probab(i,k))
            next k
        next i
    elseif selectsituation == 3 then
        // shophomeroute
        for i = 0 to 5
            for k = 0 to 10
                my.routecollectionshophome(i,k,2) = Cstr(probab(i,k))
            next k
        next i
    end if
    
    // 累積確率を用いてルーレット選択を行い、ルートを決定する
    
    // 選択するための乱数を生成する
    roulettebari = rnd()
    tmproute = ""
    for k = 0 to (ebn - 1)
        if k == 0 then
            if roulettebari <= Cdbl(gettoken(rouletteruiseki(listnumber,0),k)) then
                if selectsituation == 0 or selectsituation == 1 then
                    tmproute = route(listnumber,Cint(gettoken(rouletteruiseki(listnumber,1),k)))
                    break
                else
                    tmproute = route(listnumber,Cint(gettoken(rouletteruiseki(listnumber,1),k)))
                    break
                end if
            end if
        elseif k > 0 then
                if roulettebari >= Cdbl(gettoken(rouletteruiseki(listnumber,0),k-1)) and roulettebari < Cdbl(gettoken(rouletteruiseki(listnumber,0),k)) then
                	if selectsituation == 0 or selectsituation == 1 then
                        tmproute = route(listnumber,Cint(gettoken(rouletteruiseki(listnumber,1),k)))
                        my.routebridgepoint(0) = universe.map.bridgecore(Cint(gettoken(rouletteruiseki(listnumber,1),k))).beginpointid
                        my.routebridgepoint(1) = Cint(gettoken(rouletteruiseki(listnumber,1),k))
                        break
                	else 
                        tmproute = route(listnumber,Cint(gettoken(rouletteruiseki(listnumber,1),k)))
                        my.routebridgereturnpoint(0) = universe.map.bridgecore(Cint(gettoken(rouletteruiseki(listnumber,1),k))).endpointid
                        my.routebridgereturnpoint(1) = Cint(gettoken(rouletteruiseki(listnumber,1),k))
                		break
            		end if
        		end if
        	end if
    next k
    return(tmproute)
}