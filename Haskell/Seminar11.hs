{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Seminar11 where

import Control.Applicative (liftA2)

import Utils

--------------------------------------------------------------------------------
-- КОМБИНАЦИЯ ЭФФЕКТОВ

-- Каждая из изученных нами монад имеет какой-то один эффект: например, запись
-- в лог, завершение с ошибкой. Однако на практике часто удобнее оперировать
-- вычислениями, комбинирующими в себе несколько эффектов.

-- Самый жизненный пример: тип Future (Maybe a), - то есть канал для получения
-- результата асинхронного вычисления, которое может не найти ничего (Future сам
-- по себе может вернуть исключение, поэтому никаких Either не требуется).

-- Наша цель - научиться удобно работать с типами вида m1 (m2 a), где m1, m2 -
-- две монады.

--------------------------------------------------------------------------------
-- КОМПОЗИЦИЯ НА УРОВНЕ ТИПОВ

-- Когда мы изучали класс типов Traversable, то объявляли тип Compose,
-- реализующий композицию на уровне типов:
newtype Compose f g a = Compose (f (g a)) deriving (Eq, Show)

-- Мы выяснили, что Compose двух функторов является функтором, а Compose двух
-- аппликативных функторов - аппликативный функтор. С помощью Compose,
-- таким образом, можно комбинировать несколько аппликативных эффектов.

-- Давайте реализуем Compose для монад, и тогда дело сделано.

--------------------------------------------------------------------------------
-- КОМПОЗИЦИЯ ФУНКТОРОВ

-- Начинаем с функторов:
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap h (Compose x) = Compose (fmap (fmap h) x)

-- Проверяем типы для fmap:
-- x :: f (g a)
-- => fmap (fmap h) :: f (g a) -> f (g b)
-- => fmap h :: g a -> g b
-- => h :: a -> b, - то есть все сходится.

-- Проверим выполнение первого закона: fmap id y = y:
-- fmap id y =
-- = fmap id (Compose x) =        -- конструктор Compose
-- = Compose (fmap (fmap id) x) = -- определение fmap
-- = Compose (fmap id x) =        -- закон для g
-- = Compose x                    -- заком для f
-- = y

-- Проверим выполнение второго закона: fmap h2 (fmap h1 y) = fmap (h2 . h1) y:
-- fmap h2 (fmap h1 y) =
-- = fmap h2 (fmap h1 (Compose x)) =               - конструктор Compose
-- = fmap h2 (Compose (fmap (fmap h1) x)) =        - опредеделение fmap
-- = Compose (fmap (fmap h2) (fmap (fmap h1) x)) = - определение fmap
-- = Compose (fmap (fmap h2 . fmap h1) x) =        - закон для g
-- = Compose (fmap (fmap (h2 . h1)) x) =           - закон для f
-- = fmap (h2 . h1) (Compose x) =                  - определение fmap
-- = fmap (h2 . h1) y

-- Таким образом, композиция функторов действительно является функторов.

-- Теперь мы можем проносить фукнцию сквозь несколько контейнеров:
_ = (^ 2) <$> Compose [Just 3, Nothing] `is`
    Compose [Just 9, Nothing]

_ = (^ 2) <$> Compose [Compose $ Right $ Just 2] `is`
    Compose [Compose $ Right $ Just 4]

--------------------------------------------------------------------------------
-- КОМПОЗИЦИЯ АППЛИКАТИВНЫХ ФУНКТОРОВ

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose (pure (pure x))

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose h <*> Compose x = Compose (fmap (<*>) h <*> x)

-- Проверяем типы для pure:
-- x :: a
-- => Compose (pure (pure x)) :: Compose f g a
-- => pure (pure x) :: f (g a)
-- => pure x :: g a, - то есть все сходится.

-- Проверяем типы для (<*>):
-- h :: f (g (a -> b)), x :: f (g a)
-- => fmap (<*>) h :: f (g a -> g b)
-- => fmap (<*>) h <*> x :: f (g b), - то есть все сходится.

-- Доказывать законы в этот раз не будем, посмотрим на примере, и то только
-- для первого закона.

-- Identity: pure id <*> ys = ys
_ = pure id <*> Compose [Just 1] `is`
    Compose (pure (pure id)) <*> Compose [Just 1] `is`
    Compose [pure id] <*> Compose [Just 1] `is`
    Compose [Just id] <*> Compose [Just 1] `is`
    Compose (fmap (<*>) [Just id] <*> [Just 1]) `is`
    Compose ([(Just id <*>)] <*> [Just 1]) `is`
    Compose [Just id <*> Just 1] `is`
    Compose [Just 1]

-- Для напоминания список оставшихся законов.
-- Homomorphism: pure f <*> pure x = pure (f x)
-- Interchange: fs <*> pure x = pure ($ x) <*> fs
-- Composition: pure (.) <*> fs <*> gs <*> xs = fs <*> (gs <*> xs)

-- Пример комбинации эффектов:
_ = Compose [Just (^2), Nothing] <*> Compose [Just 2, Just 3] `is`
    Compose [Just 4, Just 9, Nothing, Nothing]

--------------------------------------------------------------------------------
-- КОМПОЗИЦИЯ МОНАД

instance (Monad m2, Monad m1) => Monad (Compose m2 m1) where
    (>>=) :: Compose m2 m1 a -> (a -> Compose m2 m1 b) -> Compose m2 m1 b
    Compose x >>= k = undefined

-- Оказывается, реализовать инстанс Compose для монад невозможно. Дело в том,
-- что x :: m2 (m1 a), поэтому при монадическом связывании со стрелкой Клейсли
-- в общем случае получим, что x >>= k' :: m2 b', где b' - произвольный тип,
-- не обязательно монада.

-- Но нам нужно получить m2 (m1 b), и единственный способ это сделать для
-- произвольной монады m1 - это стрелка Клейсли return. Мало того, что она
-- построит тривиальный контейнер, так у нас еще и нет способа просунуть return
-- к вложенному типу.

-- Поэтому композиция на уровне типов позволяет комбинировать только
-- аппликативные эффекты. Монады слишком глубоко используют структуру
-- контейнера, поэтому для произвольных двух монад написать инстанс Compose
-- не удается.

-- Нужно придумать другой механизм, который позволит записывать композицию
-- монад.

--------------------------------------------------------------------------------
-- ТРАНСФОРМЕРЫ МОНАД

-- Трансформеры монад - это специальные варианты стандартных монад, которые
-- позволяют комбинировать данную монаду с какой-либо другой. Вторая монада
-- указывается как тип-параметр для конструирования первой монады.

-- Например, для стандартной монады Writer существует трансформер WriterT,
-- который в качестве параметра принимает другую монаду, например, Reader.

-- Если трансформер монад является экземпляром класса типа Monad, то для него
-- доступна do-нотация, и в ней можно использовать как интерфейс обеих монад.
-- Например, для трансформера WriterT монады Reader можно писать так:
getSecondW :: WriterT String (Reader [String]) String
getSecondW = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (head . tail)
    tell el1
    return el2

-- Здесь WriterT String означает, что во внешней монаде Writer тип лога -
-- строка, Reader [String] - что во внутренней монаде Reader тип окружения -
-- список строк, а String - что результат всего вычисления - строка.

-- Функция getSecondW возвращает второй элемент из окружения, занося первый
-- (который мы пропустили), в лог.

-- Для обращения к внутренней монаде нужно использовать специальную функцию
-- lift, определеннуя для монадических трансформеров, которая позволяет поднять
-- вычисление из внутренней монады во внешнюю.

_ = runReader (runWriterT getSecondW) ["fst", "snd"] `is` ("snd", "fst")

-- Можно было бы записать то же самое и наоборот, поменяв монады местами:
getSecondR :: ReaderT [String] (Writer String) String
getSecondR = do
    el1 <- asks head
    el2 <- asks (head . tail)
    lift $ tell el1
    return el2

_ = runWriter (runReaderT getSecondR ["fst", "snd"]) `is` ("snd", "fst")

-- Стоит заметить, что здесь лог и окружение друг от друга не зависят, поэтому
-- используя такие комбинации Writer и Reader нельзя получить монаду State.

--------------------------------------------------------------------------------
-- КОНСТРУКТОР READERT

-- Реализуем теперь трансформер ReaderT:
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- Обратите внимание, что кайнд ReaderT :: * -> (* -> *) -> * -> *, в то время
-- как у монады Reader :: * -> * -> * - трансформер принимает дополнительный
-- параметр кайнда * -> * - монаду, с которой нужно будет комбинировать эффекты.

-- Замечание: здесь можно было бы попробовать и сделать
-- runReaderT :: m (r -> a), но не факт, что из этого что-то хорошее получится.

-- Такое определение дает нам функцию runReaderT :: ReaderT r m a -> r -> m a,
-- с помощью которой, как и раньше, можно запускать монадические вычисления.

-- Реализуем экземпляр класса типов Monad для трансформера ReaderT.

--------------------------------------------------------------------------------
-- ФУНКТОР READERT

-- Сделаем трансформер ReaderT представителем класса типов Functor:
instance Functor m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f rx = ReaderT $ fmap f . runReaderT rx

-- Проверяем типы:
-- rx :: ReaderT r m a => runReaderT rt :: r -> m a
-- f :: a -> b         => fmap f :: m a -> m b
-- => fmap f . runReaderT rx :: r -> m b

_ = runReaderT (fmap (+ 1) rt) 2 `is` [3, 5, 7] where
    rt = ReaderT $ \ e -> [e, e * 2, e * 3]

--------------------------------------------------------------------------------
-- АППЛИКАТИВНЫЙ ФУНКТОР READERT

-- Следующий шаг к реализации - это аппликативный функтор.
instance Applicative m => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure x = ReaderT $ \ _ -> pure x

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    rf <*> rx = ReaderT $ liftA2 (<*>) (runReaderT rf) (runReaderT rx)
    -- Напоминание: liftA2 h f g = \ x -> h (f x) (g x)

-- Реализация достаточно тривиальная: имея аппликативный функтор для внутренней
-- монады, можно просто воспользоваться функциями pure и (<*>) для нее, только
-- для этого надо каждый аргумент вычислить с помощью runReaderT.

_ = runReaderT (fr <*> xr) 2 `is` [4, 8, 4, 6] where
    fr = ReaderT $ \ e -> [(e *), (e +)]
    xr = ReaderT $ \ e -> [e, e * 2]

--------------------------------------------------------------------------------
-- МОНАДА READERT

instance Monad m => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    rx >>= k = ReaderT $ \ e -> do -- Вычисление в произвольной монаде m!
        x <- runReaderT rx e       -- runReaderT rx e :: m a => x :: a
        runReaderT (k x) e        -- :: m b

_ = runReaderT (rx >>= k) 2 `is` [5, 6, 6, 8] where
    rx = ReaderT $ \ e -> [e + 1, e * 2]
    k x = ReaderT $ \ e -> [e + x, e * x]

-- Почему у нас не получалось написать Monad для Compose? Дело в том, что в
-- случае композиции обе наши монады абстрактные, а здесь мы внешнюю монаду
-- зафиксировали, и таким образом, мы знаем, как с ней работать.

--------------------------------------------------------------------------------
-- КЛАСС ТИПОВ MONADTRANS

-- Для завершения реализации не хватает всего одной детали: инструментария
-- для подъема вычислений из внутренней монады в трансформер, - то есть функции
-- lift. Она объявлена для класса типов MonadTrans.

class MonadTrans t where
    lift :: Monad m => m a -> t m a

-- Обратите внимание, что t :: (* -> *) -> *.

-- Для данного типа должны быть выполнены следующие законы:
-- 1. lift . return = return
-- 2. lift (mx >>= k) = lift mx >>= (lift . k)

-- Или в обычной нотации:
-- 1. lift (return x) = return x
-- 2. lift (mx >>= k) = lift mx >>= (\ x -> lift (k x))

--------------------------------------------------------------------------------
-- ТРАНСФОРМЕР READERT

-- Реализация функции ReaderT для трансформера ReaderT совершенно естественна:
-- нужно просто выполнить вычисление во внутренней монаде. Окружение при этом
-- игнорируется, так как внутренняя монада о нем ничего не знает.

instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift mx = ReaderT $ \ _ -> mx

_ = runReaderT rt 2 `is` [7, 8, 9, 10] where
    rt = do
        e <- ask
        x <- lift [e * 2, e * 3]
        y <- lift [e + 1, e + 2]
        return (x + y)

--------------------------------------------------------------------------------
-- ТРАНСФОРМАЦИЯ IDENTITY

-- Вспомним, что у нас есть монада Identity, которая не обладает никакими
-- эффектами:
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    Identity x >>= k = k x

-- И еще у нас есть монада Reader, которая позволяет читать из окружения.

-- Наконец, мы познакомились с трансформером ReaderT, который позволяет
-- добавлять к эффекту внунтренней монады эффект чтения из окружения.

-- А что такое ReaderT r Identity? Получается, что мы добавили возможность
-- читать из лог для монады, которая никакими эффектами не обладает, то есть
-- по содержанию ReaderT r Identity в точности эквивалентен монаде Reader r.

-- Это означает, что реализации монад Reader, Writer и State, которые были
-- рассмотрены на прошлом семинаре, на самом деле не требуются: достаточно
-- трансформировать монаду Identity.

-- Именно такой подход и выбран в стандартной библиотеке.

--------------------------------------------------------------------------------
-- МОНАДА READER

-- Соответственно, сам Reader объявляется как алиас на ReaderT вокруг Identity:
type Reader r = ReaderT r Identity

-- Но теперь нужно написать функцию для запуска reader'а:
runReader :: Reader r a -> r -> a
runReader r e = runIdentity (runReaderT r e)

-- И функцию для конструирования reader'а:
reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

-- Также Reader раньше имел стандартный интерфейс: функции ask, asks, local, -
-- который позволял читать из окружения, доставать часть окружения или выполнять
-- вычисление в локально модифицированном окружении. Их реализации тоже придется
-- поменять. Они теперь должны работать независимо от трансформированной монады.

ask :: Monad m => ReaderT r m r
ask = ReaderT return

asks :: Monad m => (r -> a) -> ReaderT r m a
asks = reader

local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f r = ReaderT $ \ e -> runReaderT r (f e)

-- Примеры использования встречались выше: мы уже пользовались этими функциями.

--------------------------------------------------------------------------------
-- КОНСТРУКТОР WRITERT

-- По аналогии с ReaderT реализуем трансформер WriterT.

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- Обратите внимание, что тип m здесь обернут вокруг всей пары, которая была
-- в монаде Writer. Это сделано потому, что так будет проще получать значения
-- из монад: достаточно будет взять композицию run-функций.

-- Общее правило следующее: трансформируемая монада оборачивает результат
-- всей функции (то есть если тип - функция, то оборачиваем ее результат, а если
-- не функция, то все, что есть).

-- Как обычно, кайнд типа WriterT :: * -> (* -> *) -> * -> *.

-- Не забываем, что для WriterT нужны еще функция получения значения и лога.
-- В случае трансформера они будут обернуты во внутреннюю монаду, поэтому
-- нужно использовать fmap:
evalWriterT :: Functor m => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

execWriterT :: Functor m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

--------------------------------------------------------------------------------
-- ФУНКТОР WRITERT

instance Functor m => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f w = WriterT $ fmap update (runWriterT w)
        where update ~(x, log) = (f x, log)
        -- Используем ленивый образец. Реализация с энергичным сопоставлением
        -- с образцом называется strict writer.

_ = runWriterT (fmap (+ 1) wt) `is` [(2, "a"), (3, "b")] where
    wt = WriterT $ [(1, "a"), (2, "b")]

--------------------------------------------------------------------------------
-- АППЛИКАТИВНЫЙ ФУНКТОР WRITERT

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT $ pure (x, mempty)

    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    wf <*> wx = WriterT $ liftA2 update (runWriterT wf) (runWriterT wx)
        where update ~(f, w) ~(x, w') = (f x, w `mappend` w')

_ = runWriterT (wf <*> wx) `is`
    [(1, "f1x1"), (2, "f1x2"), (3, "f2x1"), (4, "f2x2")] where
        wf = WriterT $ [(fst, "f1"), (snd, "f2")]
        wx = WriterT $ [((1, 3), "x1"), ((2, 4), "x2")]

--------------------------------------------------------------------------------
-- МОНАДА WRITERT

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    wx >>= k = WriterT $ do
        ~(x, w) <- runWriterT wx
        ~(y, w') <- runWriterT (k x)
        return (y, w `mappend` w')

-- Как и в случае с монадой Reader, здесь используется do-нотация для
-- произвольной монады m.

_ = runWriterT (wx >>= k) `is`
    [(2, "x1y1"), (3, "x1y2"), (4, "x2y1"), (6, "x2y2")] where
        wx = WriterT $ [(1, "x1"), (2, "x2")]
        k x = WriterT $ [(x * 2, "y1"), (x * 3, "y2")]

--------------------------------------------------------------------------------
-- ТРАНСФОРМЕР WRITERT

instance Monoid w => MonadTrans (WriterT w) where
    lift :: Monad m => m a -> WriterT w m a
    lift mx = WriterT $ do
        x <- mx
        return (x, mempty)

-- Здесь снова вычисление внутри абстрактной монады. Подъем вычисления монады
-- во writer в лог ничего не пишет, потому что внутренняя монада ничего не знает
-- об этом логе. Другое поведение было бы немного нелогично, да и нарушало бы
-- законы для MonadTrans.

_ = runWriterT wt `is` [(1, "2"), (3, "2"), (2, "3"), (4, "3")] where
    wt = do
        x <- WriterT [(2, "2"), (3, "3")]
        f <- lift [pred, succ]
        return (f x)

--------------------------------------------------------------------------------
-- МОНАДА WRITER

-- Осталось только с помощью трансформера и Identity получить обычную монаду
-- Writer:
type Writer w = WriterT w Identity

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

execWriter :: Writer w a -> w
execWriter = snd . runWriter

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

-- Наконец, остался только стандартный интерфейс.
tell :: Monad m => w -> WriterT w m ()
tell w = writer ((), w)

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen mx = WriterT $ do
    ~(x, w) <- runWriterT mx
    return ((x, w), w)

--------------------------------------------------------------------------------
-- КОНСТРУКТОР STATET

-- Последний трансформер, который мы сегодня разберем, - это StateT, то есть
-- трансформер, позволяющий на произвольную монаду навесить изменяемое
-- состояние (изменяемое в том смысле, что следующему вычислению передается
-- измененое состояние, а не то же самое).

-- По ней пробежимся очень быстро, здесь нет ничего принципиально нового.

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- Как и раньше, нужны функции для извлечения значения и состояния.

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT mx = fmap fst . runStateT mx

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT mx = fmap snd . runStateT mx

--------------------------------------------------------------------------------
-- ФУНКТОР STATET

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f sx = StateT $ \ s -> fmap update $ runStateT sx s
    where update ~(x, s') = (f x, s')

--------------------------------------------------------------------------------
-- АППЛИКАТИВНЫЙ ФУНКТОР STATET

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \ s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  sf <*> sx = StateT $ \ s -> do
      ~(f, s') <- runStateT sf s
      ~(x, s'') <- runStateT sx s'
      return (f x, s'')

--------------------------------------------------------------------------------
-- МОНАДА STATET

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  mx >>= k  = StateT $ \ s -> do
    ~(x, s') <- runStateT mx s
    runStateT (k x) s'

--------------------------------------------------------------------------------
-- ТРАНСФОРМЕР STATET

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift mx = StateT $ \ s -> do
    x <- mx
    return (x, s)

--------------------------------------------------------------------------------
-- МОНАДА STATE

type State w = StateT w Identity

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

get :: Monad m => StateT s m s
get = state $ \ s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \ _ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)

--------------------------------------------------------------------------------
-- ИНСТАНСЫ MONADFAIL

-- Трансформеры можно легко делать экземплярами MonadFail, если вложенная
-- монада является представителем этого класса типа:

instance MonadFail m => MonadFail (ReaderT r m) where
    fail :: String -> ReaderT r m a
    fail message = ReaderT $ \ _ -> fail message

instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
    fail :: String -> WriterT w m a
    fail = WriterT . fail

instance MonadFail m => MonadFail (StateT s m) where
    fail :: String -> StateT s m a
    fail message = StateT $ \ _ -> fail message

_ = runWriterT w `is` [((), "snd")] where
    w = do
     1 <- WriterT [(0, "fst"), (1, "snd")]
     return ()
-- Там, где сопоставление с образцом не удалось, взялся fail для списка, то есть
-- пустой список. В лог записано только сообщение из второго элемента, где
-- сопоставление было успешно.

--------------------------------------------------------------------------------
-- ПРОБЛЕМА ЛИФТИНГА

-- К сожалению, текущая реализация трансформеров монад имеет один серьезный
-- недостаток: для доступа к интерфейсу внетренней монаде необходимо явным
-- образом вызывать функцию lift.

-- Это может стать критично, если у нас друг на друга накручено 2, 3 и более
-- монад: тогда программисту обязательно нужно знать, как именно устроена
-- внутри монада, с которой он работает.

-- Например, возьмем такую сложную монаду:
type ComplexMonad a = StateT Int (WriterT String (Reader [String]))

-- Она умеет читать из общего окружения типа [String], писать в лог строчки и
-- имеет изменяемое состояние типа Int.

-- Разумеется, нам бы хотелось просто экспортировать ее из модуля, а в
-- do-нотации звать уже просто tell, ask, get, put безо всяких лифтов.

-- Таким образом, мы скроем реализацию нашей монады от пользователя вообще.

-- Поэтому наша следующая цель - неявный лифтинг. Эта задача сложная, но
-- решаемая. Для этого нужно познакмится с двумя расширениями языка.

--------------------------------------------------------------------------------
-- МУЛЬТИПАРАМЕТРИЧЕСКИЕ КЛАССЫ ТИПОВ

-- Для реализации неявного лифтинга нам понадобится расширение языка
-- MultiParamTypeClasses, которое позволяет создавать классы типов с несколькими
-- параметрами.

-- Идейно такие классы типов описывают взаимосвязь между несколькими типами.

-- Пример использования - можно определить арифметические операции для чисел
-- разных типов.

class Summable a b c where
    (|+|) :: a -> b -> c

instance Summable Integer Integer Integer where (|+|) = (+)
instance Summable Double Double Double where (|+|) = (+)
instance Summable Integer Double Double where i |+| f = fromIntegral i + f
instance Summable Double Integer Double where f |+| i = f + fromIntegral i

-- К сожалению, это приводит к проблеме поиска определения подходящей функции.
-- Например, если мы просто теперь запишем (2 :: Integer) |+| (2 :: Double), то
-- окажется, что тип этого выражения - Summable Integer Double c => c, то есть
-- результат.

-- Несмотря на то, что у нас есть один экземпляр для Summable Integer Double c,
-- выражение не компилируется, и требуется явно указывать тип результата.

--------------------------------------------------------------------------------
-- ФУНКЦИОНАЛЬНЫЕ ЗАВИСИМОСТИ

-- Для решения этой проблемы можно воспользовать расширением языка
-- FunctionalDependencies, которое позволяет при объявлении классов типов
-- указывать функциональные зависимости между параметрами класса типов (его
-- включение также автоматически включает MultiParamTypeClasses).

-- Пример:
class Multiplicable a b c | a b -> c where
    (|*|) :: a -> b -> c

-- Данная функциональная зависимость означает, что типы a и b однозначно
-- определяют тип c.

instance Multiplicable Integer Integer Integer where (|*|) = (*)
instance Multiplicable Double Double Double where (|*|) = (*)
instance Multiplicable Integer Double Double where i |*| f = fromIntegral i * f
instance Multiplicable Double Integer Double where f |*| i = f * fromIntegral i

-- Таким образом, теперь компилятор будет однозначно выводить тип выражений:
_ = (2 :: Integer) |*| (2 :: Double) -- `is` (4 :: Double)

-- Более того, объявление такого экземпляра приведет к ошибке компиляции
-- "Functional dependencies conflict":
-- instance Multiplicable Integer Integer Double where i1 |*| i2 = undefined

--------------------------------------------------------------------------------
-- ИНТЕРФЕЙСЫ МОНАД

-- Идея для неявного лифтинга следующая: раз мы хотим, чтобы интерфейс
-- внутренней монады был доступен без использования функции lift, то эти функции
-- нужно делать не свободными, а определенными для некоторого класса типов.

-- Таким образом, каждая монада, содержащая в себе, например, эффект записи в
-- лог, должна быть экземпляром класса типов, определяющая эти операции.

-- Иными словами: если внутренняя монада предоставляет интерфейс, то и внешняя
-- монада должна предоставлять тот же интерфейс.

-- Единственная проблема будет заключаться в том, что для n разных монад
-- в итоге нужно будет написать n ^ 2 экземпляров. Тем не менее, почти все они
-- тривиальны, поэтому эта работа не очень сложная, если интерфейс не меняется.

--------------------------------------------------------------------------------
-- КЛАСС ТИПОВ MONADREADER

-- Чтобы избежать конфликта имен с уже объявленными функциями, будем
-- новые, не требующие явного лифтинга, называть со штрихами.

-- Объявляем мультипараметрический класс типов с функциональной зависимостью:
-- тип монады определяет тип окружения.
class Monad m => MonadReader r m | m -> r where
    reader' :: (r -> a) -> m a
    ask'    :: m r
    asks'   :: (r -> a) -> m a
    local'  :: (r -> r) -> m a -> m a

-- Обобщение произошло следующим образом: в типы всех функций вместо ReaderT r m
-- мы написали просто m.

--------------------------------------------------------------------------------
-- READERT КАК MONADREADER

-- Тривиально выражается стандартный интерфейс для WriterT, потому что они
-- уже были реализованы явно.
instance Monad m => MonadReader r (ReaderT r m) where
    reader' :: (r -> a) -> ReaderT r m a
    reader' = reader

    ask' :: ReaderT r m r
    ask' = ask

    asks' :: (r -> a) -> ReaderT r m a
    asks' = asks

    local':: (r -> r) -> ReaderT r m a -> ReaderT r m a
    local' = local

-- Отсюда ясно, зачем была добавлена функциональная зависимость: тип окружения
-- на самом деле уже был задан в типе m, поэтому, чтобы запретить конфликты,
-- мы и поставили ограничение.

-- Здесь пришлось добавить еще одно расширение языка - FlexibleInstances, -
-- знакомое нам с прошлого семинара.

--------------------------------------------------------------------------------
-- WRITERT КАК MONADREADER

-- Чуть сложнее реализуется экземпляр для WriterT.
instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
    reader' :: (r -> a) -> WriterT w m a
    reader' = lift . reader'

    ask' :: WriterT w m r
    ask' = lift ask'

    asks' :: (r -> a) -> WriterT w m a
    asks' = lift . asks'

    -- В отличие от функций выше, здесь нельзя просто сделать лифтинг, поскольку
    -- качестве аргумента представляется другая монада. Это выражение нужно
    -- явно вычислить.
    local':: (r -> r) -> WriterT w m a -> WriterT w m a
    local' f wx = WriterT $ local' f (runWriterT wx)

-- Кроме того, тут можно заметить, что тип ReaderT r m не определяет тип лога,
-- то есть формально здесь нарушение функциональной зависимости. На самом деле
-- код выше даже не компилируется, но мы включили очередное расширение языка -
-- UndecidableInstances.

--------------------------------------------------------------------------------
-- КЛАСС ТИПОВ MONADWRITER

-- По аналогии здесь тип монады определяет тип лога:
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    writer' :: (a, w) -> m a
    tell'   :: w -> m ()
    listen' :: m a -> m (a, w)

-- И здесь вместо WriterT w m мы написали просто m.

--------------------------------------------------------------------------------
-- WRITERT КАК MONADWRITER

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    writer' :: (a, w) -> WriterT w m a
    writer' = writer

    tell' :: w -> WriterT w m ()
    tell' = tell

    listen' :: WriterT w m a -> WriterT w m (a, w)
    listen' = listen

--------------------------------------------------------------------------------
-- READERT КАК MONADWRITER

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    writer' :: (a, w) -> ReaderT r m a
    writer' = lift . writer'

    tell' :: w -> ReaderT r m ()
    tell' = lift . tell'

    listen' :: ReaderT r m a -> ReaderT r m (a, w)
    listen' rx = ReaderT $ \ e -> listen' (runReaderT rx e)

--------------------------------------------------------------------------------
-- НЕЯВНЫЙ ЛИФТИНГ

-- Теперь можно вернуться к нашему примеру и переписать его с неявными
-- лифтингом:
getSecondR' :: ReaderT [String] (Writer String) String
getSecondR' = do
    el1 <- asks' head
    el2 <- asks' (head . tail)
    tell' el1 -- Без lift!
    return el2

_ = runWriter (runReaderT getSecondR' ["fst", "snd"]) `is` ("snd", "fst")

--------------------------------------------------------------------------------
-- ТРАНСФОРМЕР LISTT

-- К сожалению, иногда для монад возникает небольшая проблема, которую можно
-- продемострировать на трансформере ListT.

newtype ListT m a = ListT { runListT :: m [a] }

instance Functor m => Functor (ListT m) where
    fmap :: (a -> b) -> ListT m a -> ListT m b
    fmap f lxs = ListT $ fmap (fmap f) (runListT lxs)

instance Applicative m => Applicative (ListT m) where
    pure :: a -> ListT m a
    pure x = ListT $ pure (pure x)

    (<*>) :: ListT m (a -> b) -> ListT m a -> ListT m b
    lfs <*> lxs = ListT $ liftA2 (<*>) (runListT lfs) (runListT lxs)

instance Monad m => Monad (ListT m) where
    (>>=) :: ListT m a -> (a -> ListT m b) -> ListT m b
    lxs >>= k = ListT $ do
        xs <- runListT lxs         -- xs  :: [a]
        let lys = fmap k xs        -- lys :: [ListT m b]
        let ys = fmap runListT lys -- ys  :: [m [b]]
        let mys = sequence ys      -- my  :: m [[b]]
        fmap concat mys

--------------------------------------------------------------------------------
-- ТРАНСФОРМЕР МОНАД КАК МОНАДА

-- Оказывается, не любые две монады при комбинации выдают корректную монаду.
-- Например, для монады ListT [] нарушется закон ассоциативности монадического
-- связывания:

k :: Char -> ListT [] Char
k '0' = ListT ["01"]
k '1' = ListT ["0", "1"]

_ = runListT (k '0' >>= k >>= k) `is`
    ["01001", "01101", "0100", "0101", "0110", "0111"]

_ = runListT (k '0' >>= \ c -> k c >>= k) `is`
    ["01001", "0100", "0101", "01101", "0110", "0111"]

-- Оказывается, для того, чтобы ListT m являлся монадой, требуестя, чтобы
-- внутренняя монада была коммутивной, то есть такой, чтобы порядок независимых
-- действий не влиял на результат:
-- do { a <- ma; b <- mb; m a b } = do { b <- mb; a <- ma; m a b }.

-- Такие ограничения иногда существуют, так что необходимо в общем случае
-- обязательно доказывать законы (или найти, может, кто-то уже это сделал).
