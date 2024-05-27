Imports System

Module Program
    Private board(5, 6) As Integer
    Private currentPlayer As Integer = 1

    Sub Main()
        While True
            PrintBoard()
            Console.WriteLine($"Player {currentPlayer}'s turn")
            Console.Write("Enter column (1-7): ")
            Dim input As String = Console.ReadLine()
            Dim column As Integer
            If Integer.TryParse(input, column) AndAlso column >= 1 AndAlso column <= 7 Then
                column -= 1 ' Adjust for 0-based indexing
                If DropDisc(column) Then
                    If CheckForWin(currentPlayer) Then
                        PrintBoard()
                        Console.WriteLine($"Player {currentPlayer} wins!")
                        Exit While
                    End If
                    currentPlayer = If(currentPlayer = 1, 2, 1)
                Else
                    Console.WriteLine("Column is full, try again.")
                End If
            Else
                Console.WriteLine("Invalid input, try again.")
            End If
        End While
    End Sub

    Private Sub PrintBoard()
        Console.Clear()
        For row As Integer = 0 To 5
            For col As Integer = 0 To 6
                Dim cell As String = If(board(row, col) = 0, ".", If(board(row, col) = 1, "X", "O"))
                Console.Write($"{cell} ")
            Next
            Console.WriteLine()
        Next
        Console.WriteLine("1 2 3 4 5 6 7")
    End Sub

    Private Function DropDisc(column As Integer) As Boolean
        For row As Integer = 5 To 0 Step -1
            If board(row, column) = 0 Then
                board(row, column) = currentPlayer
                Return True
            End If
        Next
        Return False
    End Function

    Private Function CheckForWin(player As Integer) As Boolean
        For row As Integer = 0 To 5
            For col As Integer = 0 To 6
                If CheckDirection(row, col, 1, 0, player) OrElse
                   CheckDirection(row, col, 0, 1, player) OrElse
                   CheckDirection(row, col, 1, 1, player) OrElse
                   CheckDirection(row, col, 1, -1, player) Then
                    Return True
                End If
            Next
        Next
        Return False
    End Function

    Private Function CheckDirection(row As Integer, col As Integer, rowDir As Integer, colDir As Integer, player As Integer) As Boolean
        Dim count As Integer = 0
        For i As Integer = 0 To 3
            Dim newRow As Integer = row + i * rowDir
            Dim newCol As Integer = col + i * colDir

            If newRow < 0 OrElse newRow >= 6 OrElse newCol < 0 OrElse newCol >= 7 Then
                Return False
            End If

            If board(newRow, newCol) = player Then
                count += 1
            Else
                Exit For
            End If
        Next
        Return count = 4
    End Function
End Module
