#include <windows.h>
#include <windowsx.h>

#define CELL_SIZE 50
#define GRID_WIDTH 7
#define GRID_HEIGHT 6

// Global variables
int grid[GRID_HEIGHT][GRID_WIDTH] = {0};  // 0 for empty, 1 for player 1, 2 for player 2
int currentPlayer = 1;

// Function declarations
LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
void DrawGrid(HDC hdc);
void DrawPiece(HDC hdc, int row, int col);
void ClearGrid();
BOOL PlacePiece(int col);
BOOL CheckForVictory(int player);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow) {
    const char CLASS_NAME[] = "ConnectFourWindowClass";

    WNDCLASS wc = {0};
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = CLASS_NAME;
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);

    RegisterClass(&wc);

    HWND hwnd = CreateWindowEx(
        0,
        CLASS_NAME,
        "Connect Four",
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT,
        GRID_WIDTH * CELL_SIZE + 15,
        GRID_HEIGHT * CELL_SIZE + 60,
        NULL,
        NULL,
        hInstance,
        NULL
    );

    if (hwnd == NULL) {
        return 0;
    }

    ShowWindow(hwnd, iCmdShow);
    UpdateWindow(hwnd);

    MSG msg = {0};
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (int)msg.wParam;
}

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    switch (uMsg) {
        case WM_DESTROY:
            PostQuitMessage(0);
            return 0;

        case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(hwnd, &ps);
            DrawGrid(hdc);
            for (int x = 0; x < GRID_WIDTH; ++x) {
                for (int y = 0; y < GRID_HEIGHT; ++y) {
                    DrawPiece(hdc, y, x);
                }
            }
            EndPaint(hwnd, &ps);
        }
        return 0;

        case WM_LBUTTONDOWN:
        {
            int xPos = GET_X_LPARAM(lParam);
            int col = xPos / CELL_SIZE;
            if (PlacePiece(col)) {
                InvalidateRect(hwnd, NULL, TRUE);
                if (CheckForVictory(currentPlayer)) {
                    MessageBox(hwnd, currentPlayer == 1 ? "Player 1 Wins!" : "Player 2 Wins!", "Game Over", MB_OK);
                    ClearGrid();
                }
                currentPlayer = currentPlayer == 1 ? 2 : 1;
            }
        }
        return 0;
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

void DrawGrid(HDC hdc) {
    for (int x = 0; x < GRID_WIDTH; ++x) {
        for (int y = 0; y < GRID_HEIGHT; ++y) {
            Rectangle(hdc, x * CELL_SIZE, y * CELL_SIZE, (x + 1) * CELL_SIZE, (y + 1) * CELL_SIZE);
        }
    }
}

void DrawPiece(HDC hdc, int row, int col) {
    int cellX = col * CELL_SIZE + CELL_SIZE / 2;
    int cellY = row * CELL_SIZE + CELL_SIZE / 2;
    HBRUSH brush;
    if (grid[row][col] == 1) {
        brush = CreateSolidBrush(RGB(255, 0, 0)); // Red for Player 1
    } else if (grid[row][col] == 2) {
        brush = CreateSolidBrush(RGB(0, 0, 255)); // Blue for Player 2
    } else {
        return; // No piece to draw
    }
    SelectObject(hdc, brush);
    Ellipse(hdc, cellX - 20, cellY - 20, cellX + 20, cellY + 20);
    DeleteObject(brush);
}

void ClearGrid() {
    for (int i = 0; i < GRID_HEIGHT; i++) {
        for (int j = 0; j < GRID_WIDTH; j++) {
            grid[i][j] = 0;
        }
    }
}

BOOL PlacePiece(int col) {
    for (int i = GRID_HEIGHT - 1; i >= 0; i--) {
        if (grid[i][col] == 0) {
            grid[i][col] = currentPlayer;
            return TRUE;
        }
    }
    return FALSE; // Column is full
}

BOOL CheckForVictory(int player) {
    // Check horizontal locations for win
    for (int row = 0; row < GRID_HEIGHT; row++) {
        for (int col = 0; col < GRID_WIDTH - 3; col++) {
            if (grid[row][col] == player && grid[row][col + 1] == player &&
                grid[row][col + 2] == player && grid[row][col + 3] == player) {
                return TRUE;
            }
        }
    }

    // Check vertical locations for win
    for (int col = 0; col < GRID_WIDTH; col++) {
        for (int row = 0; row < GRID_HEIGHT - 3; row++) {
            if (grid[row][col] == player && grid[row + 1][col] == player &&
                grid[row + 2][col] == player && grid[row + 3][col] == player) {
                return TRUE;
            }
        }
    }

    // Check positively sloped diaganols
    for (int row = 0; row < GRID_HEIGHT - 3; row++) {
        for (int col = 0; col < GRID_WIDTH - 3; col++) {
            if (grid[row][col] == player && grid[row + 1][col + 1] == player &&
                grid[row + 2][col + 2] == player && grid[row + 3][col + 3] == player) {
                return TRUE;
            }
        }
    }

    // Check negatively sloped diagonals
    for (int row = 3; row < GRID_HEIGHT; row++) {
        for (int col = 0; col < GRID_WIDTH - 3; col++) {
            if (grid[row][col] == player && grid[row - 1][col + 1] == player &&
                grid[row - 2][col + 2] == player && grid[row - 3][col + 3] == player) {
                return TRUE;
            }
        }
    }

    return FALSE;
}
