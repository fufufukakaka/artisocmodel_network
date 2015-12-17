// ダイクストラ計算モジュールのための初期化
Sub initialize_dijkstra()
{
	// 変数の初期化
	initialize_value()
	
	// 逆方向のLinkを作成
	set_reverse_link()

	// Streetを生成
	set_Street()	
}

// Linkのコスト計算式を定義
Function get_link_cost(myLinkAgt As Agt) As Double
{
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	
	beginPointAgt = myLinkAgt.Begin_Agt
	endPointAgt = myLinkAgt.End_Agt
	
	// 2点間の距離でコスト算出
	Return(MeasureDistance(beginPointAgt.X, beginPointAgt.Y, endPointAgt.X, endPointAgt.Y, Universe.Map))
}

// 変数の初期化
Sub initialize_value()
{
	Dim linkAgtSet As AgtSet
	Dim linkAgt As Agt
	Dim pointAgtSet As AgtSet
	Dim pointAgt As Agt
	
	MakeAgtSet(linkAgtSet, Universe.Link)
	
	// pointAgt.DijkstraValueを 0.0 で初期化する
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 0.0
	Next pointAgt
}

// 逆方向のLinkを作成
Sub set_reverse_link()
{
	Dim linkAgtSet As AgtSet
	Dim linkAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	Dim newLinkAgt As Agt
	
	MakeAgtSet(linkAgtSet, Universe.Link)
	
	For Each linkAgt In linkAgtSet
		beginPointAgt = linkAgt.Begin_Agt
		endPointAgt = linkAgt.End_Agt
		
		// 逆方向のLinkを生成する
		newLinkAgt = CreateAgt(Universe.Link)
		newLinkAgt.Begin_Agt = linkAgt.End_Agt
		newLinkAgt.End_Agt = linkAgt.Begin_Agt
		
		// 互いのAgtをReverseLinkAgtに格納する
		newLinkAgt.ReverseLinkAgt = linkAgt
		linkAgt.ReverseLinkAgt = newLinkAgt
	Next linkAgt	
}

// Streetを生成
Sub set_Street()
{
	Dim pointAgtSet As AgtSet	
	Dim pointAgt As Agt
	Dim linkAgtSet As AgtSet	
	Dim linkAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim nearPointAgtSet As AgtSet	
	Dim nearPointAgt As Agt
	Dim lastPointAgt As Agt
	Dim tmpPointAgt As Agt
	
	Dim streetUniqueIDArray As String
	Dim streetPointsArray As String
	Dim tokenUniqueID As String
	Dim tokenPoints As String
	Dim tokenPoints2 As String
	Dim terminateArray As String
	Dim i As Integer
	Dim j As Integer
	
	MakeAgtSet(pointAgtSet, Universe.Map.Point)
	MakeAgtSet(linkAgtSet, Universe.Link)
	
	// 各PointからリンクしているPointをNearAgtSetに代入する
	For Each linkAgt In linkAgtSet
		beginPointAgt = linkAgt.Begin_Agt
		endPointAgt = linkAgt.End_Agt	
		AddAgt(beginPointAgt.NearAgtSet, endPointAgt)
	Next linkAgt
	
	// 交差点を含むネットワークの場合
	For Each pointAgt in pointAgtSet
		If CountAgtSet(pointAgt.NearAgtSet) > 2 Then
			// 交差点のとき
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// リンクしているPointから次の交差点もしくは端点まで検索する
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// 交差点もしくは端点まで、リンクをたどる
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.streetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// コスト計算
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// 経路のとき
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// 次のリンク先をセットする
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
					ElseIf CountAgtSet(nearPointAgt.NearAgtSet) == 1 Then		// 端点のとき
						nearPointAgt.streetAgt = streetAgt
						Break
					Else	
						Break
					End If
				Loop
				
				streetAgt.Points = Left(streetAgt.Points, Len(streetAgt.Points) - 1)
			Next 	nearPointAgt			
		End If
	Next pointAgt
	
	// 交差点を含まないネットワークの場合(ラインもしくは点）
	For Each pointAgt in pointAgtSet
		If (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 0) Then
			// StreedAgtが代入されておらず、かつ他点との接続がないとき
			streetAgt = CreateAgt(Universe.Street)
			streetAgt.Points = pointAgt.ID
			streetAgt.Cost = 0.0
			pointAgt.streetAgt = streetAgt
		ElseIf (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 1) Then
			// StreedAgtが代入されておらず、かつ端点のとき
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// リンクしているPointから次の交差点もしくは端点まで検索する
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// 交差点もしくは端点まで、リンクをたどる
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.streetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// コスト計算
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// 経路のとき
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// 次のリンク先をセットする
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
					ElseIf CountAgtSet(nearPointAgt.NearAgtSet) == 1 Then		// 端点のとき
						nearPointAgt.streetAgt = streetAgt
						Break
					Else	
						Break
					End If
				Loop
				
				streetAgt.Points = Left(streetAgt.Points, Len(streetAgt.Points) - 1)
			Next 	nearPointAgt
		End If
	Next pointAgt
	
	// 交差点を含まないネットワークの場合(輪っか）
	For Each pointAgt in pointAgtSet
		If (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 2) Then
			// StreedAgtが代入されておらず、かつ経路のとき
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// リンクしているPointから輪っかを一周する
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// 交差点もしくは端点まで、リンクをたどる
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.StreetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// コスト計算
										
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// 経路のとき
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// 次のリンク先をセットする
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
						
						// 開始点に到達したとき
						If CInt(nearPointAgt.StreetAgt) <> 0 Then
							streetAgt.Points = Left(streetAgt.Points, Len(streetAgt.Points) - Len(nearPointAgt.ID) - 1)
							Break
						End If				
					End If
				Loop
				
				streetAgt.Points = Left(streetAgt.Points, Len(streetAgt.Points) - 1)
			Next nearPointAgt
		End If
	Next pointAgt
	
	// 重複するStreetを削除
	MakeAgtSet(streetAgtSet, Universe.Street)
	If CountAgtSet(streetAgtSet) > 0 Then
		streetUniqueIDArray = ""
		streetPointsArray = ""
		
		For Each streetAgt In streetAgtSet
			streetUniqueIDArray = streetUniqueIDArray & CStr(streetAgt.UniqueID) & ","
			streetPointsArray = streetPointsArray & Replace(streetAgt.Points, ",", " ") & ","
		Next streetAgt	
		
		streetUniqueIDArray = Left(streetUniqueIDArray, Len(streetUniqueIDArray) - 1)
		streetPointsArray = Left(streetPointsArray, Len(streetPointsArray) - 1)
		
		terminateArray = ""
		For i=0 To CountToken(streetUniqueIDArray) - 1
			tokenPoints = GetToken(streetPointsArray, i)
			
			If CInt(tokenPoints) <> -1 Then
				For j=i+1 To CountToken(streetUniqueIDArray) - 1
					tokenPoints2 = GetToken(streetPointsArray, j)
					If CInt(tokenPoints) <> -1 Then
						If tokenPoints == tokenPoints2 Then
							terminateArray = terminateArray & GetToken(streetUniqueIDArray, i) & ","
						End If
					End If
				Next  j
			End If	
		Next i
		
		If Len(terminateArray) > 0 Then
			terminateArray = Left(terminateArray, Len(terminateArray) - 1)
			For i=0 To CountToken(terminateArray) - 1
				TerminateAgt(CAgt(CInt(GetToken(terminateArray, i))))
			Next i
		End If	
		
		// 逆方向のStreetを生成
		reverse_street()	
	End If
}

// 逆方向のStreetを生成
Sub reverse_street()
{
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim streetAgtSet2 As AgtSet
	Dim streetAgt2 As Agt
	Dim revStreetAgt  As Agt	
	Dim hitFlag As Boolean
	Dim i As Integer
	
	MakeAgtSet(streetAgtSet, Universe.Street)
	MakeAgtSet(streetAgtSet2, Universe.Street)
	
	// 逆方向のStreetを生成
	For Each streetAgt In streetAgtSet
		hitFlag = False
		For Each streetAgt2 In streetAgtSet2
			If streetAgt.Points == reverse_token(streetAgt2.Points) Then
				revStreetAgt = streetAgt2
				revStreetAgt.ReversestreetAgt = streetAgt 
				streetAgt.ReversestreetAgt = revStreetAgt
				hitFlag = True
				Break
			End If
		Next streetAgt2
		If hitFlag == False Then
			revStreetAgt = CreateAgt(Universe.Street)
			revStreetAgt.Points = reverse_token(streetAgt.Points)
			revStreetAgt.Cost = streetAgt.Cost
			revStreetAgt.ReversestreetAgt = streetAgt 
			streetAgt.ReversestreetAgt = revStreetAgt
		End If
	Next streetAgt
}

// Link配列を逆順にして返す（Linkには向きがあるので変換）
Function reverse_links(array As String) As String
{
	Dim linkAgt As Agt
	Dim reverseLinkAgt As Agt
	Dim rtnValue As String
	Dim i As Integer
	
	// Link配列を逆順に変換
	array = reverse_token(array)
	
	rtnValue = ""
	For i=0 To CountToken(array) - 1
		linkAgt = Universe.Link(CInt(GetToken(array, i)))
		reverseLinkAgt = linkAgt.ReverseLinkAgt 
		rtnValue = CStr(reverseLinkAgt.ID) & "," & rtnValue
	Next i
	rtnValue = Left(rtnValue, Len(rtnValue) - 1)

	Return(rtnValue)
}

// 分断したStreetを復旧
Sub restore_street()
{
	Dim streetBackupAgtSet As AgtSet
	Dim streetBackupAgt As Agt
	Dim pointBackupAgtSet As AgtSet
	Dim pointBackupAgt As Agt
	Dim streetAgt As Agt
	Dim pointAgt As Agt
	Dim i As Integer
	dim ki As Agt
	
	MakeAgtSet(streetBackupAgtSet, Universe.StreetBackup)
	MakeAgtSet(pointBackupAgtSet, Universe.PointBackup)
	
	// 計算前の状態に戻す
	For Each streetBackupAgt In streetBackupAgtSet
		streetAgt = streetBackupAgt.StreetAgt
		streetAgt.Points = streetBackupAgt.Points
		streetAgt.Cost = streetBackupAgt.Cost
		streetAgt.ReverseStreetAgt = streetBackupAgt.ReverseStreetAgt
	Next streetBackupAgt
	For Each pointBackupAgt In pointBackupAgtSet
		pointAgt = pointBackupAgt.PointAgt
		pointAgt.StreetAgt = pointBackupAgt.StreetAgt
	Next pointBackupAgt
	
	// 変数を初期化する
	Do While(1)
		If CountAgtSet(Universe.TemporaryStreetAgtSet) == 0 Then
			Break
		End If
		streetAgt = GetAgt(Universe.TemporaryStreetAgtSet, 0)
		TerminateAgt(streetAgt)
	Loop
	Do While(1)
		If CountAgtSet(streetBackupAgtSet) == 0 Then
			Break
		End If
		streetBackupAgt = GetAgt(streetBackupAgtSet, 0)
		TerminateAgt(streetBackupAgt)
	Loop	
	Do While(1)
		If CountAgtSet(pointBackupAgtSet) == 0 Then
			Break
		End If
		pointBackupAgt = GetAgt(pointBackupAgtSet, 0)
		TerminateAgt(pointBackupAgt)
	Loop	
}

// 指定したトークン位置までの文字列の長さを取得
Function len_token(tokenArray As String, ct As Integer) As Integer
{
	Dim rtnValue As Integer
	Dim i As Integer
	
	rtnValue = 0
	
	If ct > CountToken(tokenArray) Then
		ExitSimulationMsg("Error: len_token : tokenArray=" & tokenArray & " ct=" & ct)
	End If
		
	For i=0 To ct - 1
		rtnValue = rtnValue + Len(GetToken(tokenArray, i)) + 1
	Next i
	rtnValue = rtnValue - 1
	
	Return(rtnValue)
}

// Street配列からPoint配列を取得
Function get_route_from_street(strStreetRoute As String) As String
{
	Dim rtnValue As String
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim workCost As Double
	Dim workPoints As String
	Dim i As Integer
	
	beginPointAgt = Universe.Map.Point(CInt(GetToken(strStreetRoute,0)))
	rtnValue = CStr(beginPointAgt.ID)

	MakeAgtSet(streetAgtSet, Universe.Street)
	
	For i=0 To CountToken(strStreetRoute) - 2
		beginPointAgt = Universe.Map.Point(CInt(GetToken(strStreetRoute,i)))
		endPointAgt = Universe.Map.Point(CInt(GetToken(strStreetRoute,i+1)))
		workCost = -1
		For Each streetAgt In streetAgtSet
			If (get_begin_point(streetAgt)  == beginPointAgt) And (get_end_point(streetAgt)  == endPointAgt) Then
				If workCost == -1 Then
					workCost = streetAgt.Cost
					workPoints = Right(streetAgt.Points, Len(streetAgt.Points) - Len(CStr(beginPointAgt.ID)))
				Else
					If streetAgt.Cost < workCost Then
						workCost = streetAgt.Cost
						workPoints = Right(streetAgt.Points, Len(streetAgt.Points) - Len(CStr(beginPointAgt.ID)))
					End If
				End If
			End If
		Next streetAgt
		
		rtnValue = rtnValue & workPoints
	Next i
	
	Return(rtnValue)
}

// トークン文字列を逆順にして返す
Function reverse_token(myArray As String) As String
{
	Dim rtnValue As String
	Dim i As Integer
	
	rtnValue = ""
	For i=0 To CountToken(myArray) - 1
		rtnValue = GetToken(myArray, i) & "," & rtnValue
	Next i
	rtnValue = Left(rtnValue, Len(rtnValue) - 1)

	Return(rtnValue)
}

// Pointをテスト出力
Sub print_point()
{
	Dim pointAgtSet As AgtSet
	Dim pointAgt As Agt
	Dim streetAgt As Agt
	Dim nearAgt As Agt
	Dim i As Integer
	
	PrintLn("")
	PrintLn("Sub print_point()")
	
	MakeAgtSet(pointAgtSet, Universe.Map.Point)
	For Each pointAgt In pointAgtSet
		Print("pointAgt.ID=" & pointAgt.ID)
		If CountAgtSet(pointAgt.NearAgtSet) > 0 Then
			Print(" pointAgt.NearAgtSet= [")
			For i=0 To CountAgtSet(pointAgt.NearAgtSet) - 1
				nearAgt = GetAgt(pointAgt.NearAgtSet, i)
				Print(" " & nearAgt.ID)
			Next i
			PrintLn(" ]")
		Else
			PrintLn("")
		End If	
	Next pointAgt
}

// Linkをテスト出力
Sub print_link()
{
	Dim linkAgtSet As AgtSet
	Dim linkAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	
	PrintLn("")
	PrintLn("Sub print_link()")
	
	MakeAgtSet(linkAgtSet, Universe.Link)
	For Each linkAgt In linkAgtSet
		beginPointAgt = linkAgt.Begin_Agt
		endPointAgt = linkAgt.End_Agt
		PrintLn("linkAgt.ID=" & linkAgt.ID & " .Begin_Agt.ID=" & beginPointAgt.ID & " .End_Agt.ID=" & endPointAgt.ID)	
	Next linkAgt
}

// Streetをテスト出力
Sub print_street()
{
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim revStreetAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	
	PrintLn("")
	PrintLn("Sub print_street()")
	
	MakeAgtSet(streetAgtSet, Universe.Street)
	For Each streetAgt In streetAgtSet
		revStreetAgt = streetAgt.ReverseStreetAgt
		Print("streetAgt.ID=" & streetAgt.ID & " .Points=" & streetAgt.Points)
		Print(" .Cost=" & revStreetAgt.Cost)
		PrintLn(" .ReverseStreetAgt.ID=" & revStreetAgt.ID)
		Print("streetAgt.ID=" & streetAgt.ID & " .Points=" & streetAgt.Points)
		Print(" .Cost=" & streetAgt.Cost)
		PrintLn(" .ReverseStreetAgt.ID=" & revStreetAgt.ID)
		PrintLn("")
	Next streetAgt
}

// Streetの始点のPointを返す
Function get_begin_point(streetAgt As Agt) As Agt
{
	Dim rtnValue As Agt
	Dim streetPoints As String
	
	streetPoints = streetAgt.Points
	
	If CountToken(streetAgt.Points) == 0 Then
		ExitSimulationMsg("Error : get_begin_point : ルートが空なのでPointが取得できません：streetAgt.Points=" & streetPoints)
	End If
	
	rtnValue = GetUniqueID(Universe.Map.Point, CInt(GetToken(streetPoints, 0)))
	
	Return(rtnValue)
}

// Streetの終点のPointを返す
Function get_end_point(streetAgt As Agt) As Agt
{
	Dim rtnValue As Agt
	Dim streetPoints As String
	
	streetPoints = streetAgt.Points
	
	If CountToken(streetAgt.Points) == 0 Then
		ExitSimulationMsg("Error : get_end_point : ルートが空なのでPointが取得できません：streetAgt.Points=" & streetPoints)
	End If
	
	rtnValue = GetUniqueID(Universe.Map.Point, CInt(GetToken(streetPoints, CountToken(streetPoints) - 1)))
	
	Return(rtnValue)
}

// 指定したStreetがStreetBackupに含まれているかチェック：含まれているときFalse、含まれていないときTrue
Function check_street_backup(streetAgt As Agt) As Boolean
{
	Dim rtnValue As Boolean
	Dim streetBackupAgtSet As AgtSet
	Dim streetBackupAgt As Agt
	
	rtnValue = True
	
	MakeAgtSet(streetBackupAgtSet, Universe.StreetBackup)
	
	For Each streetBackupAgt In streetBackupAgtSet
		If streetBackupAgt.StreetAgt == streetAgt Then
			rtnValue = False
			Break
		End If
	Next streetBackupAgt
	
	Return(rtnValue)
}

// 指定したPointがPointBackupに含まれているかチェック：含まれているときFalse、含まれていないときTrue
Function check_point_backup(pointAgt As Agt) As Boolean
{
	Dim rtnValue As Boolean
	Dim pointBackupAgtSet As AgtSet
	Dim pointBackupAgt As Agt
	
	rtnValue = True
	
	MakeAgtSet(pointBackupAgtSet, Universe.PointBackup)
	
	For Each pointBackupAgt In pointBackupAgtSet
		If pointBackupAgt.PointAgt == pointAgt Then
			rtnValue = False
			Break
		End If
	Next pointBackupAgt
	
	Return(rtnValue)
}

// ダイクストラ法の計算：始点（startPointID）から複数の候補地（goalPointArray）までの最短経路を探索する
Function dijkstra(startPointID As Integer, goalPointArray As String) As String
{
	Dim startPointAgt As Agt
	Dim countNearAgtSet As Integer
	Dim startStreetAgt As Agt
	Dim pointAgtSet As AgtSet
	Dim pointAgt As Agt
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	Dim targetAgt As Agt
	Dim freeAgtSet As AgtSet
	Dim d As Double
	Dim dijkstraStreetAgt As Agt
	Dim choiceAgt As Agt
	Dim startAgt As Agt
	Dim strStreetRoute As String
	Dim goalArray As String
	Dim minValue As Double
	Dim minRoute As String
	Dim minStreetRoute As String
	Dim tmpValue As Double
	Dim i As Integer

	// 始点の位置とゴールの位置でStreetを分断する	
	separate_street(CStr(startPointID) & "," & goalPointArray)

	// 最短経路を求める
	startPointAgt = GetUniqueID(Universe.Map.Point, startPointID)
	countNearAgtSet = CountAgtSet(startPointAgt.NearAgtSet)

	targetAgt = startPointAgt
	MakeAgtSet(streetAgtSet, Universe.Street)
	
	// Streetの開始点のPointのみ、pointAgtSetに格納する
	For Each streetAgt In streetAgtSet
		beginPointAgt = get_begin_point(streetAgt)
		If GetAgtEntry(pointAgtSet, beginPointAgt) == -1 Then
			AddAgt(pointAgtSet, beginPointAgt)
		End If
	Next streetAgt
	
	// 変数の初期化処理
	DuplicateAgtSet(freeAgtSet, pointAgtSet)
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 2147483647
	Next pointAgt
	targetAgt.DijkstraValue = 0

	Do While(1)
		// ダイクストラ法を用いて計算する
		DelAgtSet2(freeAgtSet, targetAgt)
		
		If CountAgtSet(freeAgtSet) == 0 Then
			Break
		End If
		
		For Each streetAgt In streetAgtSet
			beginPointAgt = get_begin_point(streetAgt) 
		
			If beginPointAgt.ID == targetAgt.ID Then
				endPointAgt = get_end_point(streetAgt) 
				If GetAgtEntry(freeAgtSet, endPointAgt) > -1 Then
					d = targetAgt.DijkstraValue + streetAgt.Cost
					If endPointAgt.DijkstraValue > d Then
						endPointAgt.DijkstraValue = d
						endPointAgt.ChoiceAgt = beginPointAgt
					End If
				End If
			End If
		Next streetAgt
		
		SortAgtSet(freeAgtSet, "DijkstraValue", True)
		targetAgt = GetAgt(freeAgtSet, 0)
	Loop
	
	// 最短経路を取得
	DuplicateAgtSet(freeAgtSet, pointAgtSet)
	startAgt = startPointAgt
	DelAgtSet2(freeAgtSet, startAgt)
	goalArray = ""

	For i=0 To CountToken(goalPointArray) - 1
		pointAgt = Universe.Map.Point(CInt(GetToken(goalPointArray, i)))
		
		dijkstraStreetAgt = CreateAgt(Universe.DijkstraStreet)
		strStreetRoute = CStr(pointAgt.ID)
		dijkstraStreetAgt.Value = pointAgt.DijkstraValue
		
		Do While(1)
			If SpecifyAgtType(pointAgt) == 0 Then
				// ルートがつながっていない
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			choiceAgt = pointAgt.ChoiceAgt
			
			If CInt(choiceAgt) == 0 Then
				// 目的地までのネットワークがつながっていません
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			If choiceAgt.ID == startAgt.ID Then
				// 最終地点に到着
				dijkstraStreetAgt.StreetRoute = startAgt.ID & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + startAgt.DijkstraValue
				dijkstraStreetAgt.Route = get_route_from_street(dijkstraStreetAgt.StreetRoute)
				Break
			Else
				// 次の接続点へ
				pointAgt = choiceAgt
				
				strStreetRoute = Cstr(pointAgt.ID) & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + pointAgt.DijkstraValue
			End If
		Loop
		
		// Value, Routeを格納
		goalArray = goalArray & CInt(dijkstraStreetAgt.Value*100)/100 & "," & Replace(dijkstraStreetAgt.Route, "," ," ")  & ","
	Next pointAgt
	
	goalArray = Left(goalArray, Len(goalArray) - 1)
	
	// 複数のルートのうち、最短経路を取得
	minValue = CDbl(GetToken(goalArray, 0))
	minRoute = GetToken(goalArray, 1)
	If i > 1 Then
		For i=1 To CountToken(goalPointArray) - 1
			tmpValue = CDbl(GetToken(goalArray, 2*i))
			If tmpValue < minValue Then
				minValue = tmpValue
				minRoute = GetToken(goalArray, 2*i + 1)
			End If
		Next i
	End If
	
	// 分断したStreetを計算前の状態に戻す
	restore_street()
	
	// 返り値は、最短経路のPoint配列
	Return(Replace(minRoute, " ", ","))
}

// 始点と終点のPointからLinkを取得
Function get_link_from_points(beginPointID As Integer, endPointID As Integer) As Agt
{
	Dim rtnValue As Agt
	Dim linkAgtSet As AgtSet
	Dim linkAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	Dim i As Integer
	
	MakeAgtSet(linkAgtSet, Universe.Link)
	
	For Each linkAgt In linkAgtSet
		beginPointAgt = linkAgt.Begin_Agt
		endPointAgt = linkAgt.End_Agt
		If (beginPointAgt.ID == beginPointID) And (endPointAgt.ID == endPointID) Then			
			rtnValue = linkAgt
			Break
		End If
	Next linkAgt
	
	Return(rtnValue)
}

// 始点と終点のPointからStreetを取得
Function get_street_from_points(beginPointID As Integer, endPointID As Integer) As Agt
{
	Dim rtnValue As Agt
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim hitFlag As Boolean
	Dim i As Integer
	
	MakeAgtSet(streetAgtSet, Universe.Street)
	hitFlag = False
	For Each streetAgt In streetAgtSet
		For i=0 To CountToken(streetAgt.Points) - 2
			If (CInt(GetToken(streetAgt.Points, i)) == beginPointID) And (CInt(GetToken(streetAgt.Points, i+1)) == endPointID) Then
				hitFlag = True
				Break
			End If
		Next i
		If hitFlag == True Then
			rtnValue = streetAgt
			Break
		End If
	Next streetAgt
	
	Return(rtnValue)
}

// Point配列のコストを計算
Function get_points_cost(myPoints As String) As Double
{
	Dim rtnValue As Double
	Dim beginPointID As Integer
	Dim endPointID As Integer
	Dim linkAgt As Agt	
	Dim i As Integer
	
	rtnValue = 0.0
	
	For i = 0 To CountToken(myPoints) - 2
		beginPointID = CInt(GetToken(myPoints, i))
		endPointID = CInt(GetToken(myPoints, i+1))
		linkAgt = get_link_from_points(beginPointID, endPointID)
		rtnValue = rtnValue + get_link_cost(linkAgt)		// コスト計算
	Next i
	
	Return(rtnValue)
}

// Streetを分断
Sub separate_street(targetArray As String)
{
	Dim targetAgt As Agt
	Dim streetAgtSet As AgtSet
	Dim countNearAgtSet As Integer
	Dim streetAgt As Agt
	Dim tokenPointID As String
	Dim tokenLinkID As String
	Dim newStreetAgt As Agt
	Dim revStreetAgt As Agt
	Dim newRevStreetAgt As Agt
	Dim pointAgt As Agt
	Dim streetBackupAgt As Agt
	Dim pointBackupAgt As Agt
	Dim i As Integer
	Dim j As Integer
	Dim k As Integer
	Dim ct As Integer

	MakeAgtSet(streetAgtSet, Universe.Street)
	
	For j=0 To CountToken(targetArray) - 1
		targetAgt = GetUniqueID(Universe.Map.Point, CInt(GetToken(targetArray, j)))
		countNearAgtSet = CountAgtSet(targetAgt.NearAgtSet)
		If countNearAgtSet == 2 Then	// Streetを中継するPointの場合、Streetを分断する
			streetAgt = targetAgt.StreetAgt
			
			For i = 0 To CountToken(streetAgt.Points) - 1
				tokenPointID = GetToken(streetAgt.Points, i)
				
				If CInt(tokenPointID) == targetAgt.ID Then				
					ct = i + 1
					
					// バックアップ処理
					If check_street_backup(streetAgt) == True Then
						streetBackupAgt = CreateAgt(Universe.StreetBackup)
						streetBackupAgt.StreetAgt = streetAgt
						streetBackupAgt.Points = streetAgt.Points
						streetBackupAgt.Cost = streetAgt.Cost
						streetBackupAgt.ReverseStreetAgt = streetAgt.ReverseStreetAgt
					End If
					
					// 前半分
					streetAgt.Points = Left(streetBackupAgt.Points, len_token(streetBackupAgt.Points, ct))
					streetAgt.Cost = get_points_cost(streetAgt.Points)
					
					// 後ろ半分
					newStreetAgt = CreateAgt(Universe.Street)
					AddAgt(Universe.TemporaryStreetAgtSet, newStreetAgt)
					newStreetAgt.Points = Right(streetBackupAgt.Points, Len(streetBackupAgt.Points) - len_token(streetBackupAgt.Points, ct - 1) -1)
					newStreetAgt.Cost = get_points_cost(newStreetAgt.Points)
					
					For k=0 To CountToken(newStreetAgt.Points) - 1
						pointAgt = GetUniqueID(Universe.Map.Point, CInt(GetToken(newStreetAgt.Points, k)))
						If check_point_backup(pointAgt) == True Then
							pointBackupAgt = CreateAgt(Universe.PointBackup)
							pointBackupAgt.PointAgt = pointAgt
							pointBackupAgt.StreetAgt = pointAgt.StreetAgt
						End If
						pointAgt.StreetAgt = newStreetAgt
					Next k

					// 逆方向のStreet
					revStreetAgt = streetAgt.ReverseStreetAgt
					
					// バックアップ処理
					If check_street_backup(revStreetAgt) == True Then
						streetBackupAgt = CreateAgt(Universe.StreetBackup)
						streetBackupAgt.StreetAgt = revStreetAgt
						streetBackupAgt.Points = revStreetAgt.Points
						streetBackupAgt.Cost = revStreetAgt.Cost
						streetBackupAgt.ReverseStreetAgt = revStreetAgt.ReverseStreetAgt
					End If
					
					ct = CountToken(streetBackupAgt.Points) - ct + 1
					
					// 逆方向の前半分
					revStreetAgt.Points = reverse_token(newStreetAgt.Points)
					revStreetAgt.Cost = get_points_cost(revStreetAgt.Points)
					
					// 逆方向の後ろ半分
					newRevStreetAgt = CreateAgt(Universe.Street)
					AddAgt(Universe.TemporaryStreetAgtSet, newRevStreetAgt)
					newRevStreetAgt.Points = reverse_token(streetAgt.Points)
					newRevStreetAgt.Cost = get_points_cost(newRevStreetAgt.Points)

					For k=0 To CountToken(newRevStreetAgt.Points) - 1
						pointAgt = GetUniqueID(Universe.Map.Point, CInt(GetToken(newRevStreetAgt.Points, k)))
						If check_point_backup(pointAgt) == True Then
							pointBackupAgt = CreateAgt(Universe.PointBackup)
							pointBackupAgt.PointAgt = pointAgt
							pointBackupAgt.StreetAgt = pointAgt.StreetAgt
						End If
						pointAgt.StreetAgt = newRevStreetAgt
					Next k
										
					// 相互リンク
					streetAgt.ReverseStreetAgt = newRevStreetAgt
					newRevStreetAgt.ReverseStreetAgt = streetAgt
					revStreetAgt.ReverseStreetAgt = newStreetAgt
					newStreetAgt.ReverseStreetAgt = revStreetAgt
					Break
				End If
			Next i
		End If
	Next j
}

// Streetのコストを更新
Sub update_cost(beginPointID As Integer, endPointID As Integer, newCost As Double)
{
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim revStreetAgt As Agt
	Dim hitFlag As Boolean
	Dim i As Integer
		
	// Streetの探索
	MakeAgtSet(streetAgtSet, Universe.Street)
	hitFlag = False
	For Each streetAgt In streetAgtSet
		For i=0 To CountToken(streetAgt.Points) - 2
			If (CInt(GetToken(streetAgt.Points, i)) == beginPointID) And (CInt(GetToken(streetAgt.Points, i+1)) == endPointID) Then
				hitFlag = True
				Break
			End If
		Next i
		If hitFlag == True Then
			Break
		End If
	Next streetAgt
	
	If hitFlag == True Then
		// コストの更新
		streetAgt.Cost = newCost
		revStreetAgt = streetAgt.ReverseStreetAgt
		revStreetAgt.Cost = streetAgt.Cost
	End If
}

// ダイクストラ法の計算：始点（startPointID）から複数の候補地（goalPointArray）までの最短経路を探索する
// 更にコストと始点と終点、またどの橋を使うのかも出力できるようにする
Function dijkstrav2(startPointID As Integer, goalPointArray As String) As String
{
	Dim startPointAgt As Agt
	Dim countNearAgtSet As Integer
	Dim startStreetAgt As Agt
	Dim pointAgtSet As AgtSet
	Dim pointAgt As Agt
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	Dim targetAgt As Agt
	Dim freeAgtSet As AgtSet
	Dim d As Double
	Dim dijkstraStreetAgt As Agt
	Dim choiceAgt As Agt
	Dim startAgt As Agt
	Dim strStreetRoute As String
	Dim goalArray As String
	Dim minValue As Double
	Dim minRoute As String
	Dim minStreetRoute As String
	Dim tmpValue As Double
	Dim i As Integer
    dim k as integer
	dim returndijkstra as string
    dim usingbridge as integer
    dim modifiedroute as string

	// 始点の位置とゴールの位置でStreetを分断する	
	separate_street(CStr(startPointID) & "," & goalPointArray)

	// 最短経路を求める
	startPointAgt = GetUniqueID(Universe.Map.Point, startPointID)
	countNearAgtSet = CountAgtSet(startPointAgt.NearAgtSet)

	targetAgt = startPointAgt
	MakeAgtSet(streetAgtSet, Universe.Street)
	
	// Streetの開始点のPointのみ、pointAgtSetに格納する
	For Each streetAgt In streetAgtSet
		beginPointAgt = get_begin_point(streetAgt)
		If GetAgtEntry(pointAgtSet, beginPointAgt) == -1 Then
			AddAgt(pointAgtSet, beginPointAgt)
		End If
	Next streetAgt
	
	// 変数の初期化処理
	DuplicateAgtSet(freeAgtSet, pointAgtSet)
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 2147483647
	Next pointAgt
	targetAgt.DijkstraValue = 0

	Do While(1)
		// ダイクストラ法を用いて計算する
		DelAgtSet2(freeAgtSet, targetAgt)
		
		If CountAgtSet(freeAgtSet) == 0 Then
			Break
		End If
		
		For Each streetAgt In streetAgtSet
			beginPointAgt = get_begin_point(streetAgt) 
		
			If beginPointAgt.ID == targetAgt.ID Then
				endPointAgt = get_end_point(streetAgt) 
				If GetAgtEntry(freeAgtSet, endPointAgt) > -1 Then
					d = targetAgt.DijkstraValue + streetAgt.Cost
					If endPointAgt.DijkstraValue > d Then
						endPointAgt.DijkstraValue = d
						endPointAgt.ChoiceAgt = beginPointAgt
					End If
				End If
			End If
		Next streetAgt
		
		SortAgtSet(freeAgtSet, "DijkstraValue", True)
		targetAgt = GetAgt(freeAgtSet, 0)
	Loop
	
	// 最短経路を取得
	DuplicateAgtSet(freeAgtSet, pointAgtSet)
	startAgt = startPointAgt
	DelAgtSet2(freeAgtSet, startAgt)
	goalArray = ""

	For i=0 To CountToken(goalPointArray) - 1
		pointAgt = Universe.Map.Point(CInt(GetToken(goalPointArray, i)))
		
		dijkstraStreetAgt = CreateAgt(Universe.DijkstraStreet)
		strStreetRoute = CStr(pointAgt.ID)
		dijkstraStreetAgt.Value = pointAgt.DijkstraValue
		
		Do While(1)
			If SpecifyAgtType(pointAgt) == 0 Then
				// ルートがつながっていない
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			choiceAgt = pointAgt.ChoiceAgt
			
			If CInt(choiceAgt) == 0 Then
				// 目的地までのネットワークがつながっていません
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			If choiceAgt.ID == startAgt.ID Then
				// 最終地点に到着
				dijkstraStreetAgt.StreetRoute = startAgt.ID & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + startAgt.DijkstraValue
				dijkstraStreetAgt.Route = get_route_from_street(dijkstraStreetAgt.StreetRoute)
				Break
			Else
				// 次の接続点へ
				pointAgt = choiceAgt
				
				strStreetRoute = Cstr(pointAgt.ID) & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + pointAgt.DijkstraValue
			End If
		Loop
		
		// Value, Routeを格納
		goalArray = goalArray & CInt(dijkstraStreetAgt.Value*100)/100 & "," & Replace(dijkstraStreetAgt.Route, "," ," ")  & ","
	Next pointAgt
	
	goalArray = Left(goalArray, Len(goalArray) - 1)
	
	// 複数のルートのうち、最短経路を取得
	minValue = CDbl(GetToken(goalArray, 0))
	minRoute = GetToken(goalArray, 1)
	If i > 1 Then
		For i=1 To CountToken(goalPointArray) - 1
			tmpValue = CDbl(GetToken(goalArray, 2*i))
			If tmpValue < minValue Then
				minValue = tmpValue
				minRoute = GetToken(goalArray, 2*i + 1)
			End If
		Next i
	End If
	
	// 分断したStreetを計算前の状態に戻す
	restore_street()
    
    // カンマ区切りのものを別に用意しておく
    modifiedroute = Replace(minRoute, " ", ",")
    
    // 初期化
    usingbridge = 0
    
    // ルートでどの橋が使われるかを調べる
    for k = 0 to 10
        if instr(1,modifiedroute,CStr(universe.Map.bridgecore(k).beginpointid) &","& CStr(universe.Map.bridgecore(k).endpointid)) >= 1 then
		usingbridge = k
        	break
	elseif instr(1,modifiedroute,CStr(universe.Map.bridgecore(k).endpointid) &","& CStr(universe.Map.bridgecore(k).beginpointid)) >= 1 then
		usingbridge = k
        	break
	end if
    next k
	
	// 返り値は、最短経路のPoint配列とそのコスト、始点と終点、そして使う橋のID
    // ルートを使う前に，区切りにすることが必要
    returndijkstra = minRoute&","&minvalue&","&gettoken(modifiedroute,0)&","&gettoken(modifiedroute,counttoken(modifiedroute)-1)&","&usingbridge
	return(returndijkstra)
}