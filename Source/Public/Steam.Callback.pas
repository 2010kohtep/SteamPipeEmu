unit Steam.Callback;

{$I Default.inc}
{$A4}

interface

// [English]
//
// How do callbacks work in steam? feat. Aleksandr Belkin aka kohtep aka Xander
//
// They work disgusting. It is not intuitively clear even in the presence of source
// codes that describe the creation of callbacks in some classes. Just a nightmare.
// There is NO normal, ordered, concise documentation on callbacks in Steam. If you
// decide to work with them, then most likely you will have to spend a lot of time,
// like me, to figure out how they function, because most of their work is hidden
// in binaries, not in source codes. You will have to reverse engineer. If you don't know how -
// It's a lost case. Therefore, I will present here a small article about how callbacks
// work and how to use them, so that I can reread and remember from time to time.
//
// There are two sources where there is more or less a description of how callbacks
// work - the steam_api.h file and the site for game developers and Valve partners
// - partner.steamgames.com (it is rare to find the use of callbacks in leaked CS:GO
// source codes, but such finds are very valuable, more on that later). I will often
// refer to these two resources of information.
//
// In short, callbacks are functions that are called at some point in the
// game. They are registered by some classes, for example, CGCClient
// registers four callbacks:
//   m_callbackGCMessageAvailable
//   m_CallbackSteamServersDisconnected
//   m_CallbackSteamServerConnectFailure
//   m_CallbackSteamServersConnected
//
// I will also attach the code for their declaration in the class description:
//
// CCallback< CGCClient, GCMessageAvailable_t, false > m_callbackGCMessageAvailable;
// STEAM_CALLBACK( CGCClient, OnSteamServersDisconnected, SteamServersDisconnected_t, m_CallbackSteamServersDisconnected );
// STEAM_CALLBACK( CGCClient, OnSteamServerConnectFailure, SteamServerConnectFailure_t, m_CallbackSteamServerConnectFailure );
// STEAM_CALLBACK( CGCClient, OnSteamServersConnected, SteamServersConnected_t, m_CallbackSteamServersConnected );
//
// I also attach the description of the STEAM_CALLBACK macro and those used in it:
//
{$REGION 'Description of macros'}
// #define _STEAM_CALLBACK_AUTO_HOOK( thisclass, func, param )
// #define _STEAM_CALLBACK_HELPER( _1, _2, SELECTED, ... )		_STEAM_CALLBACK_##SELECTED
// #define _STEAM_CALLBACK_SELECT( X, Y )						_STEAM_CALLBACK_HELPER X Y
// #define _STEAM_CALLBACK_3( extra_code, thisclass, func, param ) \
// 	struct CCallbackInternal_ ## func : private CCallbackImpl< sizeof( param ) > { \
// 		CCallbackInternal_ ## func () { extra_code SteamAPI_RegisterCallback( this, param::k_iCallback ); } \
// 		CCallbackInternal_ ## func ( const CCallbackInternal_ ## func & ) { extra_code SteamAPI_RegisterCallback( this, param::k_iCallback ); } \
// 		CCallbackInternal_ ## func & operator=( const CCallbackInternal_ ## func & ) { return *this; } \
// 		private: virtual void Run( void *pvParam ) { _STEAM_CALLBACK_AUTO_HOOK( thisclass, func, param ) \
// 			thisclass *pOuter = reinterpret_cast<thisclass*>( reinterpret_cast<char*>(this) - offsetof( thisclass, m_steamcallback_ ## func ) ); \
// 			pOuter->func( reinterpret_cast<param*>( pvParam ) ); \
// 		} \
// 	} m_steamcallback_ ## func ; void func( param *pParam )
// #define _STEAM_CALLBACK_4( _, thisclass, func, param, var ) \
// 	CCallback< thisclass, param > var; void func( param *pParam )
//
//  Declares a callback member function plus a helper member variable which
//  registers the callback on object creation and unregisters on destruction.
//  The optional fourth 'var' param exists only for backwards-compatibility
//  and can be ignored.
//  #define STEAM_CALLBACK( thisclass, func, .../*callback_type, [deprecated] var*/ ) \
//    _STEAM_CALLBACK_SELECT( ( __VA_ARGS__, 4, 3 ), ( /**/, thisclass, func, __VA_ARGS__ ) )
//
// The code above was taken from the steam_api.h file.
{$ENDREGION}
//
// It is important to note that the very first callback is declared outside of the
// STEAM_CALLBACK macro. This means a very important thing - the callback function
// is NOT SET, however, it will be set manually in the constructor. This is important
// to remember, because when I saw such code, I could not understand how it should
// work until I found the constructor.
//
// According to the documentation at partner.steamgames.com, they are called
// every game frame via the SteamAPI_RunCallbacks function (in most games).
// However, some callbacks are only called under certain conditions or events.
// For example, the m_callbackGCMessageAvailable callback of the CGCClient class
// is called only when a packet is received from the coordinator server. The
// other three are obviously fired on disconnection, connection problems, or
// successful connection to the steam server events.
//
// This is where the most important question for me arises - how does the
// callback system understand when to call this or that callback? This is
// not described ABSOLUTELY ANYWHERE. Maybe I just searched badly, but in
// my humble opinion I searched well: github, gitlab, cis and foreign forums,
// source codes of valve projects leaked to the network, early reverses and
// attempts to write an emulator - the answer to this question was never found.
//
// After spending a lot of time separating the wheat from the chaff, I came
// to the conclusion that callbacks are purely a function of the Steam platform,
// and the client should not create their own callbacks. He can, but it makes
// little sense.
//
// Nice and short excerpt from the correspondence:
//
// kohtep, [14.07.20 05:31]
// In general, callbacks are designed for developers in volvo
// Even with partial documentation for someone out there
// All the main part of their work is hidden in steamclient.dll, which is private
// You can use them (it doesn't matter if you are a game developer or a cheat maker), but as for me - it's pointless
// They are so difficult to understand and implement that it is easier to make your system
// Much simplified and without functions you don't need
//
// Having carefully studied the work of the CClientPipe::BGetCallback function,
// I came to the conclusion that callbacks can be read through the SteamPipe
// protocol. To be honest, this was a big discovery for me. Briefly - BGetCallback
// is a function that checks the queue of callbacks in the pipe. If there is a
// callback in the queue, then it reads it and gives it to the function that
// called this function. It works according to the following algorithm:
//
//  1. In the steam_api.dll module (yes, I forgot to mention that it is he who
//     initiates the callbacks), the exported function SteamAPI_RunCallbacks is called
//     by some external module, it doesn’t matter which one, it can be anything.
//
//  2. The SteamAPI_RunCallbacks function calls the CallbackMgr_RunCallbacks
//     function if the SteamPipe is initialized (g_hSteamPipe is not equal to 0).
//
//  3. The CallbackMgr_RunCallbacks function calls the CCallbackMgr::RunCallbacks method.
//
//  4. The CCallbackMgr::RunCallbacks method, while doing some checks, calls the
//     Steam_BGetCallback function, which is imported from steamclient.dll.
//
//  5. The Steam_BGetCallback function calls the CIPCClient::BGetCallbackAndDoInternalDispatch method.
//
//  6. The CIPCClient::BGetCallbackAndDoInternalDispatch method executes the
//     CClientPipe::BGetCallback method.
//
//  7. The CClientPipe::BGetCallback method ***begins polling the SteamPipe protocol***.
//
//  8. If the method has read a byte from the queue, then it checks it, and if
//     the byte is equal to 0x07, then the execution of callbacks begins. With
//     this byte, the server side of the pipes, as it were, tells the client that
//     it has callbacks to execute.
//
//  9. I'm not sure about this stage, but it looks like the method completely clears
//     the SteamPipe's incoming buffer and waits for new data.
//
// 10. The method sends byte 0x02 to the pipe. This byte is a trigger to call the
//     SerializeCallbacks method in the original steamclient.dll, in our case (Xander.SteamPipe)
//     the appropriate class will be called.
//
// 11. The CClientPipe::BGetCallback method enters the state waiting for a response.
//     It expects a CallbackMsg_t structure in the following format:
//
//       [Int8]  - Any byte (Read Ready ID)
//       [Int32] - SteamUser (SteamID low part)
//       [Int32] - Callback index (see isteamclient.h)
//       [Int32] - Callback data size
//       [...]   - Callback data, size equal to previous Int32
//
// 11.1. The original steamclient sets the 'any byte' field to 2.
// 12. After processing the received data, the method must receive the data,
//     the structure of which is described in paragraph 11, however, all fields
//     must be equal to 0 (except for the first byte, it can still be any value).
//
// The original callback execution function can only execute 32 callbacks at a
// time. If she still has callbacks left in the queue, then she will send 0x07 bytes
// again so that the client sends her a request to execute again. This will continue
// until all callbacks have been completed.
//

// [Russian]
//
// А как работают колбеки в стиме? feat. Aleksandr Belkin aka kohtep aka Xander
//
// Отвратительно работают. Интуитивно непонятно даже при наличии исходных
// кодов, в которых описано создание колбеков в некоторых классах. Просто кошмар.
// Нормальной, упорядоченной, лаконичной документации по колбекам в стиме НЕ СУЩЕСТВУЕТ.
// Если ты решился с ними поработать, то скорее всего тебе придётся, как и мне,
// потратить уйму времени, чтобы разобраться, как они функционируют, потому что
// большая часть их работы скрыта в бинарных файлах, а не в исходниках. Придётся
// реверсить. Не умеешь - пиши пропало. Поэтому я изложу здесь небольшую статейку о том,
// как работают колбеки и как ими пользоваться, чтобы время от времени перечитывать
// и вспоминать.
//
// Есть два источника, где более-менее есть описание работы колбеков - файл
// steam_api.h и сайт для разработчиков игр и партнеров Valve - partner.steamgames.com
// (редко можно найти использование колбеков в утекших исходных кодах CS:GO, но такие
// находки весьма ценны, об этом чуть позже). Я буду часто ссылаться на эти два ресурса информации.
//
// Если кратко, то колбеки - это функции, которые вызываются в какие-то моменты
// игры. Они регистрируются какими-то классами, например CGCClient регистрирует
// четыре колбека:
//   m_callbackGCMessageAvailable
//   m_CallbackSteamServersDisconnected
//   m_CallbackSteamServerConnectFailure
//   m_CallbackSteamServersConnected
//
// Также я приложу код их объявления в описании класса:
//
// CCallback< CGCClient, GCMessageAvailable_t, false > m_callbackGCMessageAvailable;
// STEAM_CALLBACK( CGCClient, OnSteamServersDisconnected, SteamServersDisconnected_t, m_CallbackSteamServersDisconnected );
// STEAM_CALLBACK( CGCClient, OnSteamServerConnectFailure, SteamServerConnectFailure_t, m_CallbackSteamServerConnectFailure );
// STEAM_CALLBACK( CGCClient, OnSteamServersConnected, SteamServersConnected_t, m_CallbackSteamServersConnected );
//
// Описание макроса STEAM_CALLBACK и используемых в нём тоже прикладываю:
//
{$REGION 'Описание макросов'}
// #define _STEAM_CALLBACK_AUTO_HOOK( thisclass, func, param )
// #define _STEAM_CALLBACK_HELPER( _1, _2, SELECTED, ... )		_STEAM_CALLBACK_##SELECTED
// #define _STEAM_CALLBACK_SELECT( X, Y )						_STEAM_CALLBACK_HELPER X Y
// #define _STEAM_CALLBACK_3( extra_code, thisclass, func, param ) \
// 	struct CCallbackInternal_ ## func : private CCallbackImpl< sizeof( param ) > { \
// 		CCallbackInternal_ ## func () { extra_code SteamAPI_RegisterCallback( this, param::k_iCallback ); } \
// 		CCallbackInternal_ ## func ( const CCallbackInternal_ ## func & ) { extra_code SteamAPI_RegisterCallback( this, param::k_iCallback ); } \
// 		CCallbackInternal_ ## func & operator=( const CCallbackInternal_ ## func & ) { return *this; } \
// 		private: virtual void Run( void *pvParam ) { _STEAM_CALLBACK_AUTO_HOOK( thisclass, func, param ) \
// 			thisclass *pOuter = reinterpret_cast<thisclass*>( reinterpret_cast<char*>(this) - offsetof( thisclass, m_steamcallback_ ## func ) ); \
// 			pOuter->func( reinterpret_cast<param*>( pvParam ) ); \
// 		} \
// 	} m_steamcallback_ ## func ; void func( param *pParam )
// #define _STEAM_CALLBACK_4( _, thisclass, func, param, var ) \
// 	CCallback< thisclass, param > var; void func( param *pParam )
//
//  Declares a callback member function plus a helper member variable which
//  registers the callback on object creation and unregisters on destruction.
//  The optional fourth 'var' param exists only for backwards-compatibility
//  and can be ignored.
//  #define STEAM_CALLBACK( thisclass, func, .../*callback_type, [deprecated] var*/ ) \
//    _STEAM_CALLBACK_SELECT( ( __VA_ARGS__, 4, 3 ), ( /**/, thisclass, func, __VA_ARGS__ ) )
//
// Код выше был взят из файла steam_api.h.
{$ENDREGION}
//
// Важно обратить внимание на то, что самый первый колбек объявлен вне макроса
// STEAM_CALLBACK. Это значит очень важную вещь - колбек-функция НЕ УСТАНОВЛЕНА,
// однако, она будет установлена вручную в конструкторе. Это важно помнить, потому
// что когда я увидел такой код, то я не мог понять, как он должен работать, пока
// не нашел конструктор.
//
// Если верить документации в partner.steamgames.com, то вызываются они каждый
// игровой кадр через функцию SteamAPI_RunCallbacks (в большинстве игр). Однако,
// некоторые колбеки вызываются только при определенных условиях или событиях.
// Так, колбек m_callbackGCMessageAvailable класса CGCClient вызывается только
// когда получен пакет от сервера-координатора. Остальные три, очевидно, вызываются
// при событиях отключения, проблемы подключения, или успешного подключения к
// серверу стима.
//
// Вот здесь и встаёт самый важдый вопрос для меня - а как система вызова колбеков
// понимает, когда нужно вызвать тот или иной колбек? Это не описано АБСОЛЮТНО НИГДЕ.
// Возможно, я просто плохо искал, но на мой скромный взгляд я искал хорошо: гитхаб,
// гитлаб, снг и зарубежные форумы, исходные коды утекших в сеть проектов вальв,
// ранние реверсы и попытки написания эмулятора - ответ на этот вопрос так и не был найден.
//
// Потратив кучу времени на отделение зерен от плевел я пришел к выводу, что
// колбеки - это функционал сугубо платформы Steam, и клиент не должен создавать
// собственные колбеки. Он может, но смысла мало.
//
// Хорошая и краткая выдержка из переписки:
//
// kohtep, [14.07.20 05:31]
// В общем колбеки созданы для девелоперов в вольв
// Даже при учёте частичной документации для кого-то там
// Вся основная часть их работы спрятана в steamclient.dll, который приватный
// Ими можно пользоваться (неважно, гейм девелопер ты или чит мейкер), но как по мне - бессмысленно
// Они настолько сложны в понимании и реализации, что проще сделать свою систему
// Намного упрощенную и без ненужных тебе функций
//
// Внимательно изучив работу функции CClientPipe::BGetCallback я пришел к выводу,
// что колбеки могут читаться через SteamPipe протокол. Если честно, то для меня
// это оказалось большим открытием. Кратко - BGetCallback это функция, которая
// проверяет очередь колбеков в пайпе. Если в очереди есть колбек, то она читает
// его и отдаёт в функцию, которая эту функцию вызвала. Работает это по следующему
// алгоритму:
//
//  1. В модуле steam_api.dll (да, забыл упомянуть, что вызов колбеков инициирует
//     именно он) вызывается экспортируемая функция SteamAPI_RunCallbacks каким-то
//     модулем извне, каким - неважно, он может быть любым.
//  2. Функция SteamAPI_RunCallbacks вызывает CallbackMgr_RunCallbacks функцию
//     при условии, если SteamPipe инициализирован (g_hSteamPipe не равен 0).
//  3. Функция CallbackMgr_RunCallbacks вызывает метод CCallbackMgr::RunCallbacks.
//  4. Метод CCallbackMgr::RunCallbacks, выполняя некоторые проверки, вызывает
//     Steam_BGetCallback функцию, которая импортируется из steamclient.dll.
//  5. Функция Steam_BGetCallback вызывает метод CIPCClient::BGetCallbackAndDoInternalDispatch.
//  6. Метод CIPCClient::BGetCallbackAndDoInternalDispatch выполняет метод
//     CClientPipe::BGetCallback.
//  7. Метод CClientPipe::BGetCallback ***начинает опрашивать SteamPipe протокол***.
//  8. Если метод прочитал байт из очереди, то он производит его проверку и если
//     байт равняется 0x07, то начинается выполнение колбеков. Этим байтом серверная
//     часть пайпов как бы говорит клиенту, что у неё есть колбеки для выполнения.
//  9. Не уверен насчёт этой стадии, но судя по всему метод полностью очищает SteamPipe
//     входящий буфер и ожидает новые данные.
// 10. Метод посылает в пайп байт 0x02. Этот байт - триггер для вызова SerializeCallbacks
//     метода в оригинальном steamclient.dll, в нашем случае (Xander.SteamPipe)
//     будет вызван соответствующий класс.
// 11. Метод CClientPipe::BGetCallback входит в состояние ожидание ответа. Он
//     ожидает структуру CallbackMsg_t в следующем формате:
//
//       [Int8]  - Любой байт (идентификатор готовности к чтению)
//       [Int32] - SteamUser (нижняя часть SteamID)
//       [Int32] - Индекс колбека (см. isteamclient.h)
//       [Int32] - Размер данных колбека
//       [...]   - Данные колбека, размер равен предыдущему Int32
//
// 11.1. Оригинальный steamclient в поле 'любой байт' устанавливает 2.
// 12. После обработки полученных данных, метод должен получить данные, структура
//     которых описана в 11 пункте, однако все поля должны равняться 0 (кроме
//     первого байта, он всё ещё может быть любым значением).
//
// Оригинальная функция выполнения колбеков может выполнить только 32 колбека за
// раз. Если у неё ещё остались колбеки в очереди, то она отправит 0x07 байт ещё раз,
// чтобы клиент прислал ей запрос на выполнение ещё раз. Так будет продолжаться, пока
// все колбеки не будут выполнены.
//

type
  CallbackMsg_t = record
    SteamUser: Integer;
    Callback: Integer;
    ParamData: Pointer;
    ParamSize: Integer;

    class function Create(SteamUser: Cardinal; CallbackIndex: Integer; Data: Pointer; Size: Integer): CallbackMsg_t; static;
  end;
  PCallbackMsg = ^TCallbackMsg;
  TCallbackMsg = CallbackMsg_t;

implementation

{ CallbackMsg_t }

class function CallbackMsg_t.Create(SteamUser: Cardinal; CallbackIndex: Integer;
  Data: Pointer; Size: Integer): CallbackMsg_t;
begin
  Result.SteamUser := SteamUser;
  Result.Callback := CallbackIndex;

  if (Size <= 0) or (Data = nil) then
  begin
    Result.ParamData := nil;
    Result.ParamSize := 0;
  end
  else
  begin
    Result.ParamData := GetMemory(Size);
    Move(Data^, Result.ParamData^, Size);
    Result.ParamSize := Size;
  end;
end;

end.
