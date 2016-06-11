## T1 - kwiat vs grzyb, T2 - emocja, neutralna, szczęśliwa, przestraszona

library(stringr)
if(interactive())source('~/cs/code/r/tasks/task/task.R')
TASK.NAME <<- 'ablinkjozwiakowska'

FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 120
MAX.REACTION.TIME = 3000
ISI = 16
NOF.ITEMS = 15
BLOCK.LENGTH = 24
KEYS <<- c(Key.Left, Key.Right)

## WINDOW$set.visible(T)
## WINDOW$set.mouse.cursor.visible(T)

## Wczytujemy wszystkie obrazki
i = new(Image)
load.jpgs = function(folder){
    files = dir(folder)
    pictures = list()
    for(f in files){
        i$load.from.file(paste(folder, f, sep = ''))
        s = new(Sprite)
        tekstura = new(Texture)
        tekstura$create(i$size[1], i$size[2])
        tekstura$load.from.image(i, c(0, 0, i$size))
        s$set.texture(tekstura, F)
        pictures[[length(pictures) + 1]] = c(s, tekstura)
        center(pictures[[length(pictures)]][[1]], WINDOW)
    }
    pictures
}

flowers = load.jpgs('./flowers/')
mushrooms = load.jpgs('./mushrooms/')
faces = load.jpgs('./faces180/')
fearful = load.jpgs('./fearful/')
happy = load.jpgs('./happy/')
neutral = load.jpgs('./neutral/')
## Wszystkie w jednej liście do łatwiejszego wybierania
pictures = list(flowers = flowers, mushrooms = mushrooms, faces = faces, fearful = fearful, happy = happy, neutral = neutral)

FX = fixation(WINDOW, size = .02)

trial.code = function(trial, t1type = sample(c('mushrooms', 'flowers'), 1), t1pos = sample(c(3, 6, 8), 1),
    t2type = sample(c('neutral', 'happy', 'fearful'), 1), t2lag = sample(c(1, 3, 5, 7), 1), feedback = 0){
    t1type = as.character(t1type)
    t2type = as.character(t2type)
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    t2pos = t1pos + t2lag
    ## Mieszamy indeksy obrazków
    flowers.i = sample(1:length(flowers))
    mushrooms.i = sample(1:length(mushrooms))
    faces.i = sample(1:length(faces))
    happy.i = sample(1:length(happy))
    neutral.i = sample(1:length(neutral))
    fearful.i = sample(1:length(fearful))
    ## Wszystkie indeksy losowe w jednej liście, żeby było łatwiej wybierać
    indices = list(flowers = flowers.i, mushrooms = mushrooms.i, faces = faces.i, happy = happy.i, neutral = neutral.i, fearful = fearful.i)
    if(trial == 1){
        state = 'press-space'
    }else if((trial %% BLOCK.LENGTH) == 0){
        state = 'break'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Możliwość wyjścia z etapu za pomocą ESC
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Naciśnij spację")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'break' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Krótka przerwa - odpocznij. Aby kontynuować, naciśnij spację")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
                item = 1
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            if(item == t1pos){
                t1index = sample(indices[[t1type]], 1)
                i = pictures[[t1type]][[t1index]][[1]]
            }else if(item == t2pos){
                t2index = sample(indices[[t2type]], 1)
                i = pictures[[t2type]][[t2index]][[1]]
            }else{
                i = faces[[faces.i[item]]][[1]]
            }
            WINDOW$draw(i)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'stim-present'
        }, 'stim-present' = {
            if((CLOCK$time - stim.onset) >= PRESENTATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'stim-cleared'
            }
        }, 'stim-cleared' = {
            if((CLOCK$time - stim.cleared) >= ISI){
                if(item <= NOF.ITEMS){
                    item = item + 1
                    state = 'show-stim'
                }else{
                    state = 'measure-reaction'
                }
            }
        }, 'measure-reaction' = {
            WINDOW$set.visible(F)
            choice1 = gui.choose.item(c('KWIAT', 'GRZYB'))
            choice2 = gui.choose.item(c('NEUTRALNA', 'SZCZĘŚLIWA', 'PRZESTRASZONA'))
            WINDOW$set.visible(T)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            ## if(!is.null(ACC) || ((CLOCK$time - redgreen.onset) > MAX.REACTION.TIME)){
            if(feedback == 1){
                feedback.onset = CLOCK$time
                state = 'feedback'
            }else{
                state = 'done'
            }
            ## }
        }, 'feedback' = {
            if((CLOCK$time - feedback.onset) < FEEDBACK.TIME){
                WINDOW$clear(c(.5, .5, .5))
                ## TXT$set.string(c('Źle', 'Dobrze', 'Za późno')[ifelse(is.null(ACC), 3, ACC + 1)])
                TXT$set.string("Brak feedbacku")
                WINDOW$draw(center.win(TXT))
                WINDOW$display()
            }else{
                state = 'done'
            }
        }, 'done' = {
            WINDOW$clear(c(.5, .5, .5))
            WINDOW$display()
            return(list(r1 = choice1, r2 = choice2, t1 = t1index, t2 = t2index))
            ## return(list(rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - redgreen.onset),
            ##             acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Proszę wyłączyć telefon komórkowy.
W razie jakichkolwiek wątpliwości proszę nie wołać osoby prowadzącej, tylko podnieść do góry rękę.
Osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień. 
Badanie jest anonimowe.

Za chwilę trzeba będzie wpisać dane osobowe: wiek, płeć oraz pseudonim.
Pseudonim składa się z inicjałów (małymi literami) oraz czterech cyfr:
dnia i miesiąca urodzenia (np.  ms0706).")
gui.user.data()

gui.show.instruction("Teraz rozpocznie się zadanie rozpoznawania zdjęć przedstawiających kwiaty lub grzyby i zdjęć, na których widać twarz wyrażającą radość, strach lub twarz neutralną. Zadanie to składa się z serii prób, w trakcie których na ekranie komputera prezentowane są szybko, jedno po drugim, różne obrazy. I tak dalej...

Po zakończeniu prezentacji zdjęć pojawi się okno pozwalające zaznaczyć, czy w serii zdjęć pojawił się kwiat, czy grzyb. Następnie pojawi się okno pozwalające zaznaczyć, jaka emocja była prezentowana na ekranie.

Najpierw będzie można wykonać zadanie w wersji treningowej. Ten etap będzie krótki i jego celem będzie zapoznanie się z przebiegiem zadania.")

run.trials(trial.code, record.session = F, expand.grid(t1type = c('mushrooms', 'flowers'), t1pos = c(3, 6, 8),
    t2type = c('neutral', 'happy', 'fearful'), t2lag = c(1, 3, 5, 7)), condition = 'default', nof.trials = 8)

gui.show.instruction("Teraz rozpocznie się zadanie w wersji właściwej. Ten etap potrwa dłużej.")

run.trials(trial.code, record.session = T, condition = 'default',
           expand.grid(t1type = c('mushrooms', 'flowers'), t1pos = c(3, 6, 8),
                       t2type = c('neutral', 'happy', 'fearful'), t2lag = c(1, 3, 5, 7)),
           b = 1) ## 76 warunków

gui.show.instruction("Dziękujemy za udział w badaniu. Proszę poczekać na swoim miejscu, aż osoba prowadząca badanie podejdzie i poinformuje o dalszym postępowaniu.")

if(!interactive())quit("no")
