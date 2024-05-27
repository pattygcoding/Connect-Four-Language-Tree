Imports System.Windows
Imports System.Windows.Controls
Imports System.Windows.Media
Imports System.Windows.Shapes

Namespace ConnectFour
    Public Partial Class MainWindow
        Inherits Window

        Private board(5, 6) As Integer
        Private currentPlayer As Integer = 1

        Public Sub New()
            InitializeComponent()
        End Sub

        Private Sub DropDisc_Click(sender As Object, e As RoutedEventArgs)
            Dim column As Integer = Grid.GetColumn(CType(sender, UIElement))

            For row As Integer = 5 To 0 Step -1
                If board(row, column) = 0 Then
                    board(row, column) = currentPlayer

                    Dim disc As New Ellipse With {
                        .Width = 50,
                        .Height = 50,
                        .Fill = If(currentPlayer = 1, Brushes.Red, Brushes.Yellow)
                    }
                    Canvas.SetLeft(disc, column * 100 + 25)
                    Canvas.SetTop(disc, row * 100 + 25)
                    GameBoard.Children.Add(disc)

                    If CheckForWin(currentPlayer) Then
                        MessageBox.Show($"Player {currentPlayer} wins!")
                    End If

                    currentPlayer = If(currentPlayer = 1, 2, 1)
                    Exit For
                End If
            Next
        End Sub

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
    End Class
End Namespace
