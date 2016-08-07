// �_�C�N�X�g���v�Z���W���[���̂��߂̏�����
Sub initialize_dijkstra()
{
	// �ϐ��̏�����
	initialize_value()
	
	// �t������Link���쐬
	set_reverse_link()

	// Street�𐶐�
	set_Street()	
}

// Link�̃R�X�g�v�Z�����`
Function get_link_cost(myLinkAgt As Agt) As Double
{
	Dim beginPointAgt As Agt
	Dim endPointAgt As Agt
	
	beginPointAgt = myLinkAgt.Begin_Agt
	endPointAgt = myLinkAgt.End_Agt
	
	// 2�_�Ԃ̋����ŃR�X�g�Z�o
	Return(MeasureDistance(beginPointAgt.X, beginPointAgt.Y, endPointAgt.X, endPointAgt.Y, Universe.Map))
}

// �ϐ��̏�����
Sub initialize_value()
{
	Dim linkAgtSet As AgtSet
	Dim linkAgt As Agt
	Dim pointAgtSet As AgtSet
	Dim pointAgt As Agt
	
	MakeAgtSet(linkAgtSet, Universe.Link)
	
	// pointAgt.DijkstraValue�� 0.0 �ŏ���������
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 0.0
	Next pointAgt
}

// �t������Link���쐬
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
		
		// �t������Link�𐶐�����
		newLinkAgt = CreateAgt(Universe.Link)
		newLinkAgt.Begin_Agt = linkAgt.End_Agt
		newLinkAgt.End_Agt = linkAgt.Begin_Agt
		
		// �݂���Agt��ReverseLinkAgt�Ɋi�[����
		newLinkAgt.ReverseLinkAgt = linkAgt
		linkAgt.ReverseLinkAgt = newLinkAgt
	Next linkAgt	
}

// Street�𐶐�
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
	
	// �ePoint���烊���N���Ă���Point��NearAgtSet�ɑ������
	For Each linkAgt In linkAgtSet
		beginPointAgt = linkAgt.Begin_Agt
		endPointAgt = linkAgt.End_Agt	
		AddAgt(beginPointAgt.NearAgtSet, endPointAgt)
	Next linkAgt
	
	// �����_���܂ރl�b�g���[�N�̏ꍇ
	For Each pointAgt in pointAgtSet
		If CountAgtSet(pointAgt.NearAgtSet) > 2 Then
			// �����_�̂Ƃ�
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// �����N���Ă���Point���玟�̌����_�������͒[�_�܂Ō�������
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// �����_�������͒[�_�܂ŁA�����N�����ǂ�
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.streetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// �R�X�g�v�Z
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// �o�H�̂Ƃ�
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// ���̃����N����Z�b�g����
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
					ElseIf CountAgtSet(nearPointAgt.NearAgtSet) == 1 Then		// �[�_�̂Ƃ�
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
	
	// �����_���܂܂Ȃ��l�b�g���[�N�̏ꍇ(���C���������͓_�j
	For Each pointAgt in pointAgtSet
		If (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 0) Then
			// StreedAgt���������Ă��炸�A�����_�Ƃ̐ڑ����Ȃ��Ƃ�
			streetAgt = CreateAgt(Universe.Street)
			streetAgt.Points = pointAgt.ID
			streetAgt.Cost = 0.0
			pointAgt.streetAgt = streetAgt
		ElseIf (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 1) Then
			// StreedAgt���������Ă��炸�A���[�_�̂Ƃ�
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// �����N���Ă���Point���玟�̌����_�������͒[�_�܂Ō�������
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// �����_�������͒[�_�܂ŁA�����N�����ǂ�
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.streetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// �R�X�g�v�Z
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// �o�H�̂Ƃ�
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// ���̃����N����Z�b�g����
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
					ElseIf CountAgtSet(nearPointAgt.NearAgtSet) == 1 Then		// �[�_�̂Ƃ�
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
	
	// �����_���܂܂Ȃ��l�b�g���[�N�̏ꍇ(�ւ����j
	For Each pointAgt in pointAgtSet
		If (CInt(pointAgt.StreetAgt) == 0) And (CountAgtSet(pointAgt.NearAgtSet) == 2) Then
			// StreedAgt���������Ă��炸�A���o�H�̂Ƃ�
			DuplicateAgtSet(nearPointAgtSet, pointAgt.NearAgtSet)
			
			For Each nearPointAgt In nearPointAgtSet
				// �����N���Ă���Point����ւ������������
				streetAgt = CreateAgt(Universe.Street)
				streetAgt.Points = pointAgt.ID & ","
				streetAgt.Cost = 0.0
				lastPointAgt = pointAgt

				Do While(1)
					// �����_�������͒[�_�܂ŁA�����N�����ǂ�
					streetAgt.Points = streetAgt.Points & nearPointAgt.ID & ","
					For Each linkAgt In linkAgtSet
						If (linkAgt.Begin_Agt == lastPointAgt) And (linkAgt.End_Agt == nearPointAgt) Then
							Break
						End If
					Next linkAgt
					linkAgt.StreetAgt = streetAgt
					streetAgt.Cost = streetAgt.Cost + get_link_cost(linkAgt)		// �R�X�g�v�Z
										
					If CountAgtSet(nearPointAgt.NearAgtSet) == 2 Then		// �o�H�̂Ƃ�
						For Each tmpPointAgt In nearPointAgt.NearAgtSet
							If tmpPointAgt.ID <> lastPointAgt.ID Then
								// ���̃����N����Z�b�g����
								nearPointAgt.streetAgt = streetAgt
								lastPointAgt = nearPointAgt
								nearPointAgt = tmpPointAgt
								Break
							End If
						Next tmpPointAgt
						
						// �J�n�_�ɓ��B�����Ƃ�
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
	
	// �d������Street���폜
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
		
		// �t������Street�𐶐�
		reverse_street()	
	End If
}

// �t������Street�𐶐�
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
	
	// �t������Street�𐶐�
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

// Link�z����t���ɂ��ĕԂ��iLink�ɂ͌���������̂ŕϊ��j
Function reverse_links(array As String) As String
{
	Dim linkAgt As Agt
	Dim reverseLinkAgt As Agt
	Dim rtnValue As String
	Dim i As Integer
	
	// Link�z����t���ɕϊ�
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

// ���f����Street�𕜋�
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
	
	// �v�Z�O�̏�Ԃɖ߂�
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
	
	// �ϐ�������������
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

// �w�肵���g�[�N���ʒu�܂ł̕�����̒������擾
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

// Street�z�񂩂�Point�z����擾
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

// �g�[�N����������t���ɂ��ĕԂ�
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

// Point���e�X�g�o��
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

// Link���e�X�g�o��
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

// Street���e�X�g�o��
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

// Street�̎n�_��Point��Ԃ�
Function get_begin_point(streetAgt As Agt) As Agt
{
	Dim rtnValue As Agt
	Dim streetPoints As String
	
	streetPoints = streetAgt.Points
	
	If CountToken(streetAgt.Points) == 0 Then
		ExitSimulationMsg("Error : get_begin_point : ���[�g����Ȃ̂�Point���擾�ł��܂���FstreetAgt.Points=" & streetPoints)
	End If
	
	rtnValue = GetUniqueID(Universe.Map.Point, CInt(GetToken(streetPoints, 0)))
	
	Return(rtnValue)
}

// Street�̏I�_��Point��Ԃ�
Function get_end_point(streetAgt As Agt) As Agt
{
	Dim rtnValue As Agt
	Dim streetPoints As String
	
	streetPoints = streetAgt.Points
	
	If CountToken(streetAgt.Points) == 0 Then
		ExitSimulationMsg("Error : get_end_point : ���[�g����Ȃ̂�Point���擾�ł��܂���FstreetAgt.Points=" & streetPoints)
	End If
	
	rtnValue = GetUniqueID(Universe.Map.Point, CInt(GetToken(streetPoints, CountToken(streetPoints) - 1)))
	
	Return(rtnValue)
}

// �w�肵��Street��StreetBackup�Ɋ܂܂�Ă��邩�`�F�b�N�F�܂܂�Ă���Ƃ�False�A�܂܂�Ă��Ȃ��Ƃ�True
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

// �w�肵��Point��PointBackup�Ɋ܂܂�Ă��邩�`�F�b�N�F�܂܂�Ă���Ƃ�False�A�܂܂�Ă��Ȃ��Ƃ�True
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

// �_�C�N�X�g���@�̌v�Z�F�n�_�istartPointID�j���畡���̌��n�igoalPointArray�j�܂ł̍ŒZ�o�H��T������
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

	// �n�_�̈ʒu�ƃS�[���̈ʒu��Street�𕪒f����	
	separate_street(CStr(startPointID) & "," & goalPointArray)

	// �ŒZ�o�H�����߂�
	startPointAgt = GetUniqueID(Universe.Map.Point, startPointID)
	countNearAgtSet = CountAgtSet(startPointAgt.NearAgtSet)

	targetAgt = startPointAgt
	MakeAgtSet(streetAgtSet, Universe.Street)
	
	// Street�̊J�n�_��Point�̂݁ApointAgtSet�Ɋi�[����
	For Each streetAgt In streetAgtSet
		beginPointAgt = get_begin_point(streetAgt)
		If GetAgtEntry(pointAgtSet, beginPointAgt) == -1 Then
			AddAgt(pointAgtSet, beginPointAgt)
		End If
	Next streetAgt
	
	// �ϐ��̏���������
	DuplicateAgtSet(freeAgtSet, pointAgtSet)
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 2147483647
	Next pointAgt
	targetAgt.DijkstraValue = 0

	Do While(1)
		// �_�C�N�X�g���@��p���Čv�Z����
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
	
	// �ŒZ�o�H���擾
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
				// ���[�g���Ȃ����Ă��Ȃ�
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			choiceAgt = pointAgt.ChoiceAgt
			
			If CInt(choiceAgt) == 0 Then
				// �ړI�n�܂ł̃l�b�g���[�N���Ȃ����Ă��܂���
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			If choiceAgt.ID == startAgt.ID Then
				// �ŏI�n�_�ɓ���
				dijkstraStreetAgt.StreetRoute = startAgt.ID & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + startAgt.DijkstraValue
				dijkstraStreetAgt.Route = get_route_from_street(dijkstraStreetAgt.StreetRoute)
				Break
			Else
				// ���̐ڑ��_��
				pointAgt = choiceAgt
				
				strStreetRoute = Cstr(pointAgt.ID) & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + pointAgt.DijkstraValue
			End If
		Loop
		
		// Value, Route���i�[
		goalArray = goalArray & CInt(dijkstraStreetAgt.Value*100)/100 & "," & Replace(dijkstraStreetAgt.Route, "," ," ")  & ","
	Next pointAgt
	
	goalArray = Left(goalArray, Len(goalArray) - 1)
	
	// �����̃��[�g�̂����A�ŒZ�o�H���擾
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
	
	// ���f����Street���v�Z�O�̏�Ԃɖ߂�
	restore_street()
	
	// �Ԃ�l�́A�ŒZ�o�H��Point�z��
	Return(Replace(minRoute, " ", ","))
}

// �n�_�ƏI�_��Point����Link���擾
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

// �n�_�ƏI�_��Point����Street���擾
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

// Point�z��̃R�X�g���v�Z
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
		rtnValue = rtnValue + get_link_cost(linkAgt)		// �R�X�g�v�Z
	Next i
	
	Return(rtnValue)
}

// Street�𕪒f
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
		If countNearAgtSet == 2 Then	// Street�𒆌p����Point�̏ꍇ�AStreet�𕪒f����
			streetAgt = targetAgt.StreetAgt
			
			For i = 0 To CountToken(streetAgt.Points) - 1
				tokenPointID = GetToken(streetAgt.Points, i)
				
				If CInt(tokenPointID) == targetAgt.ID Then				
					ct = i + 1
					
					// �o�b�N�A�b�v����
					If check_street_backup(streetAgt) == True Then
						streetBackupAgt = CreateAgt(Universe.StreetBackup)
						streetBackupAgt.StreetAgt = streetAgt
						streetBackupAgt.Points = streetAgt.Points
						streetBackupAgt.Cost = streetAgt.Cost
						streetBackupAgt.ReverseStreetAgt = streetAgt.ReverseStreetAgt
					End If
					
					// �O����
					streetAgt.Points = Left(streetBackupAgt.Points, len_token(streetBackupAgt.Points, ct))
					streetAgt.Cost = get_points_cost(streetAgt.Points)
					
					// ��딼��
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

					// �t������Street
					revStreetAgt = streetAgt.ReverseStreetAgt
					
					// �o�b�N�A�b�v����
					If check_street_backup(revStreetAgt) == True Then
						streetBackupAgt = CreateAgt(Universe.StreetBackup)
						streetBackupAgt.StreetAgt = revStreetAgt
						streetBackupAgt.Points = revStreetAgt.Points
						streetBackupAgt.Cost = revStreetAgt.Cost
						streetBackupAgt.ReverseStreetAgt = revStreetAgt.ReverseStreetAgt
					End If
					
					ct = CountToken(streetBackupAgt.Points) - ct + 1
					
					// �t�����̑O����
					revStreetAgt.Points = reverse_token(newStreetAgt.Points)
					revStreetAgt.Cost = get_points_cost(revStreetAgt.Points)
					
					// �t�����̌�딼��
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
										
					// ���݃����N
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

// Street�̃R�X�g���X�V
Sub update_cost(beginPointID As Integer, endPointID As Integer, newCost As Double)
{
	Dim streetAgtSet As AgtSet
	Dim streetAgt As Agt
	Dim revStreetAgt As Agt
	Dim hitFlag As Boolean
	Dim i As Integer
		
	// Street�̒T��
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
		// �R�X�g�̍X�V
		streetAgt.Cost = newCost
		revStreetAgt = streetAgt.ReverseStreetAgt
		revStreetAgt.Cost = streetAgt.Cost
	End If
}

// �_�C�N�X�g���@�̌v�Z�F�n�_�istartPointID�j���畡���̌��n�igoalPointArray�j�܂ł̍ŒZ�o�H��T������
// �X�ɃR�X�g�Ǝn�_�ƏI�_�A�܂��ǂ̋����g���̂����o�͂ł���悤�ɂ���
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

	// �n�_�̈ʒu�ƃS�[���̈ʒu��Street�𕪒f����	
	separate_street(CStr(startPointID) & "," & goalPointArray)

	// �ŒZ�o�H�����߂�
	startPointAgt = GetUniqueID(Universe.Map.Point, startPointID)
	countNearAgtSet = CountAgtSet(startPointAgt.NearAgtSet)

	targetAgt = startPointAgt
	MakeAgtSet(streetAgtSet, Universe.Street)
	
	// Street�̊J�n�_��Point�̂݁ApointAgtSet�Ɋi�[����
	For Each streetAgt In streetAgtSet
		beginPointAgt = get_begin_point(streetAgt)
		If GetAgtEntry(pointAgtSet, beginPointAgt) == -1 Then
			AddAgt(pointAgtSet, beginPointAgt)
		End If
	Next streetAgt
	
	// �ϐ��̏���������
	DuplicateAgtSet(freeAgtSet, pointAgtSet)
	For Each pointAgt In pointAgtSet
		pointAgt.DijkstraValue = 2147483647
	Next pointAgt
	targetAgt.DijkstraValue = 0

	Do While(1)
		// �_�C�N�X�g���@��p���Čv�Z����
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
	
	// �ŒZ�o�H���擾
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
				// ���[�g���Ȃ����Ă��Ȃ�
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			choiceAgt = pointAgt.ChoiceAgt
			
			If CInt(choiceAgt) == 0 Then
				// �ړI�n�܂ł̃l�b�g���[�N���Ȃ����Ă��܂���
				dijkstraStreetAgt.Value = 2147483647
				dijkstraStreetAgt.StreetRoute = ""
				dijkstraStreetAgt.Route = ""
				Break
			End If
			
			If choiceAgt.ID == startAgt.ID Then
				// �ŏI�n�_�ɓ���
				dijkstraStreetAgt.StreetRoute = startAgt.ID & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + startAgt.DijkstraValue
				dijkstraStreetAgt.Route = get_route_from_street(dijkstraStreetAgt.StreetRoute)
				Break
			Else
				// ���̐ڑ��_��
				pointAgt = choiceAgt
				
				strStreetRoute = Cstr(pointAgt.ID) & "," & strStreetRoute
				dijkstraStreetAgt.Value = dijkstraStreetAgt.Value + pointAgt.DijkstraValue
			End If
		Loop
		
		// Value, Route���i�[
		goalArray = goalArray & CInt(dijkstraStreetAgt.Value*100)/100 & "," & Replace(dijkstraStreetAgt.Route, "," ," ")  & ","
	Next pointAgt
	
	goalArray = Left(goalArray, Len(goalArray) - 1)
	
	// �����̃��[�g�̂����A�ŒZ�o�H���擾
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
	
	// ���f����Street���v�Z�O�̏�Ԃɖ߂�
	restore_street()
    
    // �J���}��؂�̂��̂�ʂɗp�ӂ��Ă���
    modifiedroute = Replace(minRoute, " ", ",")
    
    // ������
    usingbridge = 0
    
    // ���[�g�łǂ̋����g���邩�𒲂ׂ�
    for k = 0 to 10
        if instr(1,modifiedroute,CStr(universe.Map.bridgecore(k).beginpointid) &","& CStr(universe.Map.bridgecore(k).endpointid)) >= 1 then
		usingbridge = k
        	break
	elseif instr(1,modifiedroute,CStr(universe.Map.bridgecore(k).endpointid) &","& CStr(universe.Map.bridgecore(k).beginpointid)) >= 1 then
		usingbridge = k
        	break
	end if
    next k
	
	// �Ԃ�l�́A�ŒZ�o�H��Point�z��Ƃ��̃R�X�g�A�n�_�ƏI�_�A�����Ďg������ID
    // ���[�g���g���O�ɁC��؂�ɂ��邱�Ƃ��K�v
    returndijkstra = minRoute&","&minvalue&","&gettoken(modifiedroute,0)&","&gettoken(modifiedroute,counttoken(modifiedroute)-1)&","&usingbridge
	return(returndijkstra)
}