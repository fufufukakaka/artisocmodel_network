// ?_?C?N?X?g???v?Z???W???[?????????????
Sub initialize_dijkstra()
{
	// ??????????
	initialize_value()
	
	// ?t??????Link????
	set_reverse_link()

	// Street????
	set_Street()	
}

// Žn“_‚ÆI“_‚ÌPoint‚©‚çLink‚ðŽæ“¾
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

// Link??R?X?g?v?Z???????`
Function get_link_cost(myLinkAgt As Agt) As Double
{
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	
	beginPointAgt = myLinkAgt.Begin_Agt
	endPointAgt = myLinkAgt.End_Agt
	
	// 2?_????????R?X?g?Z?o
	Return(MeasureDistance(beginPointAgt.X, beginPointAgt.Y, endPointAgt.X, endPointAgt.Y, Universe.Map))
}

// ??????????
Sub initialize_value()
{
	Dim linkAgtSet As AgtSet
	Dim linkAgt As Agt
	Dim pointAgtSet As AgtSet
	Dim pointAgt As Agt
	
	MakeAgtSet(linkAgtSet, Universe.Link)
	
	// pointAgt.DijkstraValue?? 0.0 ???????????
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 0.0
	Next pointAgt
}

// ?t??????Link????
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
		
		// ?t??????Link????????
		newLinkAgt = CreateAgt(Universe.Link)
		newLinkAgt.Begin_Agt = linkAgt.End_Agt
		newLinkAgt.End_Agt = linkAgt.Begin_Agt
		
		// ?????Agt??ReverseLinkAgt??i?[????
		newLinkAgt.ReverseLinkAgt = linkAgt
		linkAgt.ReverseLinkAgt = newLinkAgt
	Next linkAgt	
}

// Street????
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
	
	// ?ePoint???????N???????Point??NearAgtSet?????????
	For Each linkAgt In linkAgtSet
		beginPointAgt = linkAgt.Begin_Agt
		endPointAgt = linkAgt.End_Agt	
		AddAgt(beginPointAgt.NearAgtSet, endPointAgt)
	Next linkAgt
	
	// ?????_?????l?b?g???[?N???
	For Each pointAgt in pointAgtSet
		If CountAgtSet(pointAgt.NearAgtSet) > 2 Then
			// ?????_????
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// ?????N???????Point??????????_????????[?_??????????
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// ?????_????????[?_???A?????N???????
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.streetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// ?R?X?g?v?Z
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// ?o?H????
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// ????????N?????Z?b?g????
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
					ElseIf CountAgtSet(nearPointAgt.NearAgtSet) == 1 Then		// ?[?_????
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
	
	// ?????_????????l?b?g???[?N???(???C??????????_?j
	For Each pointAgt in pointAgtSet
		If (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 0) Then
			// StreedAgt??????????????ày?A??????_?????????????
			streetAgt = CreateAgt(Universe.Street)
			streetAgt.Points = pointAgt.ID
			streetAgt.Cost = 0.0
			pointAgt.streetAgt = streetAgt
		ElseIf (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 1) Then
			// StreedAgt??????????????ày?A????[?_????
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// ?????N???????Point??????????_????????[?_??????????
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// ?????_????????[?_???A?????N???????
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.streetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// ?R?X?g?v?Z
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// ?o?H????
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// ????????N?????Z?b?g????
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
					ElseIf CountAgtSet(nearPointAgt.NearAgtSet) == 1 Then		// ?[?_????
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
	
	// ?????_????????l?b?g???[?N???(??????j
	For Each pointAgt in pointAgtSet
		If (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 2) Then
			// StreedAgt??????????????ày?A????o?H????
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// ?????N???????Point???????????????????
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// ?????_????????[?_???A?????N???????
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.StreetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// ?R?X?g?v?Z
										
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// ?o?H????
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// ????????N?????Z?b?g????
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
						
						// ?J?n?_????B???????
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
	
	// ?d??????Street????
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
			
	End If
}