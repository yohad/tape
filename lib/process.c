#include <Windows.h>

HANDLE CreateDebuggedProcess(LPCSTR lpApplicationName)
{
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInformation;

    ZeroMemory(&startupInfo, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    ZeroMemory(&processInformation, sizeof(processInformation));

    if (!CreateProcessA(
        lpApplicationName,
        NULL,
        NULL,
        NULL,
        FALSE,
        DEBUG_PROCESS,
        NULL,
        NULL,
        &startupInfo,
        &processInformation
    ))
    {
        return INVALID_HANDLE_VALUE;
    }

    return processInformation.hProcess;
}