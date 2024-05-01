Public Class Main
    Private Const Rows As Integer = 6
    Private Const Columns As Integer = 7
    Private board(Columns - 1, Rows - 1) As Label
    Private currentPlayer As Integer = 1

    Private Sub Main_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        InitializeBoard()
    End Sub

    Private Sub InitializeBoard()
        For col As Integer = 0 To Columns - 1
            For row As Integer = 0 To Rows - 1
                board(col, row) = New Label()
                board(col, row).Text = "-"
                board(col, row).Size = New Size(30, 30)
                board(col, row).Location = New Point(30 * col + 10, 30 * row + 50)
                board(col, row).BorderStyle = BorderStyle.FixedSingle
                board(col, row).TextAlign = ContentAlignment.MiddleCenter
                Me.Controls.Add(board(col, row))
            Next
        Next
    End Sub

    Private Sub Button_Click(sender As Object, e As EventArgs) Handles Button1.Click, Button2.Click, Button3.Click, Button4.Click, Button5.Click, Button6.Click, Button7.Click
        Dim col As Integer = CType(sender, Button).Name.Last() - AscW("0"c) - 1
        If Not DropPiece(col) Then
            MessageBox.Show("Column is full.")
        End If
    End Sub

    Private Function DropPiece(col As Integer) As Boolean
        For row As Integer = Rows - 1 To 0 Step -1
            If board(col, row).Text = "-" Then
                board(col, row).Text = If(currentPlayer = 1, "X", "O")
                If CheckWin(col, row) Then
                    MessageBox.Show("Player " & currentPlayer.ToString() & " wins!")
                    Return False ' Game over
                End If
                currentPlayer = 3 - currentPlayer
                Return True
            End If
        Next
        Return False ' Column is full
    End Function

    Private Function CheckWin(col As Integer, row As Integer) As Boolean
        Return CheckLine(col, row, 1, 0) OrElse CheckLine(col, row, 0, 1) OrElse
               CheckLine(col, row, 1, 1) OrElse CheckLine(col, row, 1, -1)
    End Function

    Private Function CheckLine(col As Integer, row As Integer, dCol As Integer, dRow As Integer) As Boolean
        Dim count As Integer = 1
        Dim c, r As Integer

        ' Check in one direction
        c = col + dCol
        r = row + dRow
        While c >= 0 AndAlso c < Columns AndAlso r >= 0 AndAlso r < Rows AndAlso board(c, r).Text = board(col, row).Text
            count += 1
            c += dCol
            r += dRow
        End While

        ' Check in the opposite direction
        c = col - dCol
        r = row - dRow
        While c >= 0 AndAlso c < Columns AndAlso r >= 0 AndAlso r < Rows AndAlso board(c, r).Text = board(col, row).Text
            count += 1
            c -= dCol
            r -= dRow
        End While

        Return count >= 4
    End Function
End Class
