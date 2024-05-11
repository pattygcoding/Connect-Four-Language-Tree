#include <windows.h>
#include <string>

const int ROWS = 6;
const int COLS = 7;
const int CELL_SIZE = 50;

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

class ConnectFour {
private:
    HWND hwnd;
    char buttons[ROWS][COLS];
    char currentPlayer;
    bool gameOver;
    RECT cellRects[ROWS][COLS];

public:
    ConnectFour() : hwnd(NULL), currentPlayer('X'), gameOver(false) {
        memset(buttons, ' ', sizeof(buttons));
    }

    void createWindow(HINSTANCE hInstance) {
        WNDCLASS wc = {};
        wc.lpfnWndProc = WindowProc;
        wc.hInstance = hInstance;
        wc.lpszClassName = L"ConnectFour";

        RegisterClass(&wc);

        hwnd = CreateWindowEx(
            0,
            L"ConnectFour",
            L"Connect Four",
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT, CW_USEDEFAULT, COLS * CELL_SIZE, ROWS * CELL_SIZE + 50,
            NULL,
            NULL,
            hInstance,
            this);

        ShowWindow(hwnd, SW_SHOWNORMAL);
        UpdateWindow(hwnd);
    }

    void drawBoard(HDC hdc) {
        HBRUSH redBrush = CreateSolidBrush(RGB(255, 0, 0));
        HBRUSH yellowBrush = CreateSolidBrush(RGB(255, 255, 0));
        SelectObject(hdc, redBrush);

        for (int i = 0; i < ROWS; ++i) {
            for (int j = 0; j < COLS; ++j) {
                RECT cellRect = { j * CELL_SIZE, i * CELL_SIZE, (j + 1) * CELL_SIZE, (i + 1) * CELL_SIZE };
                cellRects[i][j] = cellRect;
                FillRect(hdc, &cellRect, (buttons[i][j] == 'X') ? redBrush : (buttons[i][j] == 'O') ? yellowBrush : (HBRUSH)GetStockObject(WHITE_BRUSH));
                Rectangle(hdc, cellRect.left, cellRect.top, cellRect.right, cellRect.bottom);
            }
        }

        DeleteObject(redBrush);
        DeleteObject(yellowBrush);
    }

    void dropPiece(int col) {
        if (!gameOver && col >= 0 && col < COLS) {
            int row = -1;
            for (int i = ROWS - 1; i >= 0; --i) {
                if (buttons[i][col] == ' ') {
                    row = i;
                    break;
                }
            }
            if (row != -1) {
                buttons[row][col] = currentPlayer;
                if (checkWin(row, col)) {
                    std::wstring winnerMsg = L"Player ";
                    winnerMsg += currentPlayer;
                    winnerMsg += L" wins!";
                    MessageBox(hwnd, winnerMsg.c_str(), L"Game Over", MB_OK);
                    gameOver = true;
                } else {
                    currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
                }
                InvalidateRect(hwnd, NULL, TRUE);
            }
        }
    }

    bool checkWin(int row, int col) {
        char symbol = buttons[row][col];
        // Check horizontally
        int count = 1;
        for (int j = col - 1; j >= 0 && buttons[row][j] == symbol; --j) count++;
        for (int j = col + 1; j < COLS && buttons[row][j] == symbol; ++j) count++;
        if (count >= 4) return true;

        // Check vertically
        count = 1;
        for (int i = row - 1; i >= 0 && buttons[i][col] == symbol; --i) count++;
        for (int i = row + 1; i < ROWS && buttons[i][col] == symbol; ++i) count++;
        if (count >= 4) return true;

        // Check diagonally (from bottom-left to top-right)
        count = 1;
        for (int i = row - 1, j = col - 1; i >= 0 && j >= 0 && buttons[i][j] == symbol; --i, --j) count++;
        for (int i = row + 1, j = col + 1; i < ROWS && j < COLS && buttons[i][j] == symbol; ++i, ++j) count++;
        if (count >= 4) return true;

        // Check diagonally (from top-left to bottom-right)
        count = 1;
        for (int i = row - 1, j = col + 1; i >= 0 && j < COLS && buttons[i][j] == symbol; --i, ++j) count++;
        for (int i = row + 1, j = col - 1; i < ROWS && j >= 0 && buttons[i][j] == symbol; ++i, --j) count++;
        if (count >= 4) return true;

        return false;
    }

    void handleMouseClick(int x, int y) {
        if (!gameOver) {
            int col = x / CELL_SIZE;
            dropPiece(col);
        }
    }
};

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    ConnectFour* game = reinterpret_cast<ConnectFour*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));

    switch (uMsg) {
    case WM_CREATE: {
        CREATESTRUCT* createStruct = reinterpret_cast<CREATESTRUCT*>(lParam);
        game = reinterpret_cast<ConnectFour*>(createStruct->lpCreateParams);
        SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(game));
        break;
    }
    case WM_PAINT: {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hwnd, &ps);
        game->drawBoard(hdc);
        EndPaint(hwnd, &ps);
        break;
    }
    case WM_LBUTTONDOWN: {
        int x = LOWORD(lParam);
        int y = HIWORD(lParam);
        game->handleMouseClick(x, y);
        break;
    }
    case WM_DESTROY: {
        PostQuitMessage(0);
        break;
    }
    default:
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    ConnectFour game;
    game.createWindow(hInstance);

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return 0;
}
