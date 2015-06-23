if !exists(":Abolish")
  finish
endif

Abolish afterword{,s}                         afterward{}
Abolish anomol{y,ies}                         anomal{}
Abolish austrail{a,an,ia,ian}                 austral{ia,ian}
Abolish cal{a,e}nder{,s}                      cal{e}ndar{}
Abolish {c,m}arraige{,s}                      {}arriage{}
Abolish {,in}consistan{cy,cies,t,tly}         {}consisten{}
Abolish delimeter{,s}                         delimiter{}
Abolish {,non}existan{ce,t}                   {}existen{}
Abolish despara{te,tely,tion}                 despera{}
Abolish d{e,i}screp{e,a}nc{y,ies}             d{i}screp{a}nc{}
Abolish euphamis{m,ms,tic,tically}            euphemis{}
Abolish hense                                 hence
Abolish {,re}impliment{,s,ing,ed,ation}       {}implement{}
Abolish improvment{,s}                        improvement{}
Abolish inherant{,ly}                         inherent{}
Abolish lastest                               latest
Abolish {les,compar,compari}sion{,s}          {les,compari,compari}son{}
Abolish {,un}nec{ce,ces,e}sar{y,ily}          {}nec{es}sar{}
Abolish {,un}orgin{,al}                       {}origin{}
Abolish persistan{ce,t,tly}                   persisten{}
Abolish referesh{,es}                         refresh{}
Abolish {,ir}releven{ce,cy,t,tly}             {}relevan{}
Abolish rec{co,com,o}mend{,s,ed,ing,ation}    rec{om}mend{}
Abolish reproducable                          reproducible
Abolish resouce{,s}                           resource{}
Abolish restraunt{,s}                         restaurant{}
Abolish seperat{e,es,ed,ing,ely,ion,ions,or}  separat{}
Abolish segument{,s,ed,ation}                 segment{}
Abolish scflead     supercalifragilisticexpialidocious
Abolish Tqbf        The quick, brown fox jumps over the lazy dog
Abolish Lidsa       Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum
Abolish adn and
Abolish soem some
Abolish witdh width
Abolish tehn then
Abolish waht what
Abolish Im I'm
Abolish ot to
Abolish havin having
Abolish hightlight highlight
Abolish ypo typo
Abolish ype type
Abolish alot a lot
Abolish aswell as well
Abolish etx etc
Abolish bcak back
Abolish REmove Remove

augroup abbreviationGroup
  autocmd!

  autocmd FileType mkd :Abolish -buffer dont don't
  autocmd FileType mkd :Abolish -buffer wasnt wasn't
  autocmd FileType mkd :Abolish -buffer isnt isn't
  autocmd FileType mkd :Abolish -buffer vim Vim
  autocmd FileType mkd :Abolish -buffer ruby Ruby
  autocmd FileType mkd :Abolish -buffer thurday Thursday
  autocmd FileType mkd :Abolish -buffer monday Monday
  autocmd FileType mkd :Abolish -buffer tuesday Tuesday
  autocmd FileType mkd :Abolish -buffer wednesday Wednesday
  autocmd FileType mkd :Abolish -buffer thursday Thursday
  autocmd FileType mkd :Abolish -buffer friday Friday
  autocmd FileType mkd :Abolish -buffer saturday Saturday
  autocmd FileType mkd :Abolish -buffer sunday Sunday
  autocmd FileType mkd :Abolish -buffer mac Mac
  autocmd FileType mkd :Abolish -buffer vim Vim
  autocmd FileType mkd :Abolish -buffer haskell Haskell
  autocmd FileType mkd :Abolish -buffer google Google
augroup END

highlight clear SignColumn

imap ;dap dap@html24.net
imap ;cpr 0608901477
imap ;city Frederiksberg
imap ;ln Pedersen
imap ;fn David
imap ;tlf 42403764
imap ;ad Godthåbsvej 166, 3. tv
imap ;name David Pedersen
imap ;zip 2000
imap ;usr davidpdrsn
imap ;em david.pdrsn@gmail.com
imap ;lp http://lonelyproton.com
Abolish appartment apartment
Abolish sefl self
Abolish vodi void
Abolish TEsting Testing
Abolish seach search
Abolish hve have
Abolish reflektion reflection
Abolish availible available
Abolish neseccary necessary
Abolish nececsary necessary
Abolish marie Marie
Abolish torben Torben
Abolish ltters letters
Abolish esting testing
Abolish thast thats
Abolish opne open
Abolish everytime every time
Abolish textexpender TextExpander
Abolish snipet snippet
Abolish typomore typo more
Abolish Ihave I have
Abolish otu out
Abolish ios iOS
Abolish agaon again
Abolish boroken broken
Abolish ddi did
Abolish sercurit security
Abolish textexpander TextExpander
Abolish dont don't
Abolish tes yes
Abolish htere there
Abolish frmo from
Abolish favoritted favourited
Abolish tweetes tweets
Abolish assignemtn assignment
Abolish reivew review
Abolish tpo typo
Abolish accomodation accommodation
Abolish chaging changing
Abolish techincally technically
Abolish realize realise
Abolish thones those
Abolish snippt snippet
Abolish appndind appending
Abolish recieve receive
Abolish selectino selection
Abolish definitly definitely
imap ;ea each arm
Abolish omf OmniFocus
imap ;atm at the moment
Abolish btw by the way
imap ;lb launchbar
Abolish fb Facebook
Abolish ily I love you ❤️
Abolish texp TextExpander
Abolish kbm Keyboard Maestro
imap ;rx [runx]
Abolish someting something
imap ;date %d %b %Y
Abolish wself typeof(self) __weak weakSelf = self;
Abolish fformat [NSString stringWithFormat:@"%|"]
Abolish llog NSLog(@"%|");
imap ;tmand At the moment an Android version is not planned. I'm doing this in my spare time outside work and university so I simply don't have the time. Sorry about that.
Abolish YP YO!
Abolish cafeine caffeine
Abolish infact in fact
Abolish surprice surprise
Abolish llp Lonely Proton
Abolish thikn think
Abolish achive archive
