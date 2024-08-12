module View exposing (..)

import Bricks.Draw
import Browser
import CommonView exposing (..)
import Config
import Convert exposing (..)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Embed.Youtube
import Embed.Youtube.Attributes
import Responsive exposing (DisplayProfile)
import TabGraphics
import Theme
import Time
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    { title = "Logan Brutsche - Portfolio"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width Element.fill
            , Element.height Element.fill

            -- , robotoFont
            ]
          <|
            case model of
                Loading _ ->
                    viewLoadingMessage

                Loaded loadedModel ->
                    view loadedModel
        ]
    }


viewLoadingMessage : Element Msg
viewLoadingMessage =
    Element.el
        [ Element.centerX
        , Element.padding 20
        ]
    <|
        Element.text "(loading)"


view : LoadedModel -> Element Msg
view model =
    let
        dProfile =
            model.dProfile
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.color Theme.defaultFontColor
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Background.color Theme.bgColor
            , Element.height Element.fill
            , Element.spacing 60
            , Element.padding 30
            ]
            [ headerElement dProfile

            -- , bodyElement dProfile model.tabState model.animateTime
            , Element.el [ Element.centerX ] <| Bricks.Draw.view model.bricksModel
            ]


headerElement : DisplayProfile -> Element Msg
headerElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.centerX
            , Font.size 80
            , Font.semiBold
            , fontMontserrat
            ]
          <|
            Element.text "Logan Brutsche"

        -- , Element.el
        --     [ Element.centerX
        --     , Font.size 36
        --     ]
        --   <|
        --     Element.text "Current Work and Some Past Projects"
        ]


bodyElement : DisplayProfile -> TabState -> Time.Posix -> Element Msg
bodyElement dProfile tabState animateTime =
    let
        tabOnTop =
            case tabState of
                OnTab tab ->
                    tab

                SwitchingTo targetTab animateStartTime ->
                    let
                        progressFloat =
                            animationProgressFloat animateStartTime animateTime
                    in
                    if progressFloat < 0.5 then
                        otherTab targetTab

                    else
                        targetTab

        canvasWidth =
            1600

        tabBodyWidth =
            canvasWidth / 2

        tabSeparation =
            10

        xOffsetAbs =
            case tabState of
                OnTab _ ->
                    0

                SwitchingTo _ animateStartTime ->
                    let
                        easingFunction x =
                            if x == 1 then
                                1

                            else
                                1 - (2 ^ (-20 * x))

                        offsetMultiplier =
                            let
                                progressFloat =
                                    animationProgressFloat animateStartTime animateTime
                            in
                            if progressFloat < 0.5 then
                                easingFunction (progressFloat * 2)

                            else
                                1 - easingFunction ((progressFloat - 0.5) * 2)
                    in
                    offsetMultiplier * (tabBodyWidth / 2)

        portfolioTabEls =
            let
                tabTopWidth =
                    240
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = 2600
                , bodyTopY = 100
                , tabTopY = 10
                , fillColor = Theme.portfolioTabBackgroundColor
                , strokeColor = Theme.tabBorderColor
                , pathThickness = 6
                , cornerRadius = 40
                , tabTopStartX = (canvasWidth / 2) - tabTopWidth - (tabSeparation / 2)
                , tabTopEndX = canvasWidth / 2 - (tabSeparation / 2)
                , bodyExtendsLeft = ((tabBodyWidth / 2) - tabTopWidth) + xOffsetAbs
                , bodyExtendsRight = tabBodyWidth / 2 - xOffsetAbs
                , canvasWidth = Element.px <| canvasWidth
                }
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                 <|
                    tabElement dProfile "Past Work" PortfolioClicked
                )
                (pastWorkEl dProfile)

        currentWorkTabEls =
            let
                tabTopWidth =
                    240
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = 2600
                , bodyTopY = 100
                , tabTopY = 10
                , fillColor = Theme.currentWorkTabBackgroundColor
                , strokeColor = Theme.tabBorderColor
                , pathThickness = 6
                , cornerRadius = 40
                , tabTopStartX = canvasWidth / 2 + (tabSeparation / 2)
                , tabTopEndX = (canvasWidth / 2) + tabTopWidth + (tabSeparation / 2)
                , bodyExtendsLeft = tabBodyWidth / 2 - xOffsetAbs
                , bodyExtendsRight = ((tabBodyWidth / 2) - tabTopWidth) + xOffsetAbs
                , canvasWidth = Element.px <| canvasWidth
                }
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                 <|
                    tabElement dProfile "About Me" CurrentWorkClicked
                )
                (aboutMeEl dProfile)

        elsToStack =
            case tabOnTop of
                CurrentWork ->
                    [ portfolioTabEls.tabShape
                    , portfolioTabEls.bodyEl
                    , currentWorkTabEls.tabShape
                    , currentWorkTabEls.bodyEl
                    , portfolioTabEls.tabEl
                    , currentWorkTabEls.tabEl
                    ]

                Portfolio ->
                    [ currentWorkTabEls.tabShape
                    , currentWorkTabEls.bodyEl
                    , portfolioTabEls.tabShape
                    , portfolioTabEls.bodyEl
                    , currentWorkTabEls.tabEl
                    , portfolioTabEls.tabEl
                    ]
    in
    stackElementsInZ [ Element.centerX, Element.height Element.fill ] <| elsToStack


pastWorkEl : DisplayProfile -> Element Msg
pastWorkEl dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 35
        , Element.padding 45
        ]
        [ portfolioEntryEl dProfile
            (Just "eestisse-bg.png")
            (Element.image
                [ Element.height <| Element.px 50 ]
                { src = "eestisse-title.png"
                , description = "eestisse"
                }
            )
            "2024"
            "Solo Project"
            [ "An LLM-powered tool that explains the counter-intuitive Estonian grammar to English speakers (for example, why \"eestisse\" means \"into Estonia\"). The tool takes English or Estonian text, translates it, and explains word by word how the Estonian is constructed."
            , "The central feature was almost shockingly easy to build, partially because LLMs tend to do well with language tasks."
            , "Built with Lamdera, a fascinating and beautiful platform that extends the rock-solid type-safety of Elm into the backend. This made innovation very fast and pleasant, but made more traditional intevrations (i.e. Google Sign In, Stripe integration) harder than they'd have otherwise been."
            ]
            [ newTabLink [] "https://eestisse.ee" "eestisse.ee"
            , newTabLink [] "https://github.com/eestisse/eestisse" "github"
            ]
        , portfolioEntryEl dProfile
            (Just "coinfight-bg.png")
            (Element.image
                [ Element.height <| Element.px 50 ]
                { src = "coinfight-title.png"
                , description = "coinfight"
                }
            )
            "2022 / 2023"
            "Solo Project"
            [ "An RTS game where users fight over crypto in-game. Players must invest real crypto into their armies, which if killed is dropped onto the battlefield for anyone else to pick up, capture, and withdraw. This is a zero-sum game where the goal is to get more out than you put in. \"Like Poker, but the chips shoot at each other!\""
            , "The goal of Coinfight was to give players the tangible experience of fighting in a virtual match over real money in real time. This necessitated building a unique architecture among web3 games, where the blockchain was only used to clear deposits and withdrawals, and most money movement was tracked in the game server."
            ]
            [ newTabLink [] "https://www.youtube.com/watch?v=7tw10KUO1_U" "demo video"
            , newTabLink [] "https://medium.com/p/472636deec57" "dev blog post"
            , newTabLink [] "https://coinfight.io/" "coinfight.io"
            , newTabLink [] "https://github.com/coinop-logan/coinfight" "github"
            ]
        , portfolioEntryEl dProfile
            (Just "smokesignal-bg.png")
            (Element.image
                [ Element.height <| Element.px 50 ]
                { src = "smokesignal-title.svg"
                , description = "smokesignal"
                }
            )
            "2020 / 2021"
            "Tech Lead"
            [ "SmokeSignal was an uncensorable, global chat forum that implemented Reddit-like functionality (nested comments in topical forums) fully on the Ethereum blockchain and accessed via an Elm/Javascript frontend, hosted on IPFS. Users were able to tip each other ETH for posts, as well as burn crypto for theirs or other posts as a sort of decentralized, unfakeable alternative to upvotes."
            , "As with DAIHard, a major goal of SmokeSignal was to be both radically free and unkillable. In other words, it was to boldly challenge some constraints of traditional systems (in this case, rejecting the idea that moderation and censorship are unavoidable and necessary) while surviving any backlash this might create (lawsuits, nation-state attacks, etc)."
            , "Thus, the main technical challenge was in making something suitably decentralized so as not to not be attackable, while still integrating with traditional frameworks and services for the purposes of marketing and usability. As an example, all conversational data was stored in Ethereum contract event logs, but a traditional HTTP server was used to serve SEO information for such posts."
            ]
            [ newTabLink [] "https://medium.com/daihard-buidlers/introducing-smokesignal-da8f19bc27af" "intro post"
            , newTabLink [] "https://www.youtube.com/watch?v=pV70Q0wgnnU" "demo video"
            , newTabLink [] "https://github.com/team-toast/SmokeSignal" "github"
            ]
        , portfolioEntryEl dProfile
            (Just "daihard-bg.png")
            daihardLogoEl
            "2019 / 2020"
            "Solo Developer"
            [ "DAIHard was a crypto/fiat exchange built entirely with Ethereum smart contracts, so that there was no central server anyone could take down. The application was designed to continue to function even in adversarial jurisdictions. note this app uses no backend server at all, even for encrypted chat between users."
            , "As part of this project, I spent two months in Zimbabwe researching the viability of crypto adoption in the face of a hyperinflated currency. A summary of my findings can be found in the below-linked ZimDai paper \"ZimDai: Blueprint for an Economic Jailbreak\"."
            , "Note that visual design for this project was contracted out."
            ]
            [ newTabLink [] "https://www.youtube.com/watch?v=WR4WovM0qwg" "demo video"
            , newTabLink [] "https://medium.com/@coinop.logan/daihard-game-theory-21a456ef224e" "game theory writeup"
            , newTabLink [] "https://github.com/team-toast/DAIHard" "github"
            , newTabLink [] "https://github.com/coinop-logan/ZimDai/blob/master/whitepaper.pdf" "ZimDai paper"
            ]
        , portfolioEntryEl dProfile
            (Just "toastycoin-bg.png")
            (Element.el [ Font.size 38 ] <| Element.text "Toastycoin")
            "2017"
            "Solo Project"
            [ "Toastycoin was an experimental dapp that used \"burnable payment\" contracts on the Ethereum blockchain to allow users to contract work from strangers on the Internet, without any previous trust or association. The burnable payment contracts used game theory to facilitate this: while loss of funds was not guaranteed, what was guaranteed was that scammers attempting to game the system would be punished and would not make a profit."
            , "See the \"game theory writeup\" link under the DAIHard project above, to read more about this game theory, as DAIHard was simply a narrowed use-case of the burnable payments developed for Toastycoin."
            , "All work (burnable payment smart contract, Javascript webpage interface) done by me."
            ]
            [ newTabLink [] "https://medium.com/@coinop.logan/preventing-scammer-profit-with-burnable-payments-ad2e9b632ef2" "Burnable Payments proposal"
            , newTabLink [] "https://medium.com/@coinop.logan/toasted-money-part-2-b5dfd0b1e946" "experiment conclusion"
            , newTabLink [] "https://github.com/coinop-logan/toastycoin" "github"
            ]
        ]


aboutMeEl : DisplayProfile -> Element Msg
aboutMeEl dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 35
        , Element.padding 45
        ]
        [ ytVidEl dProfile

        -- , Element.paragraph [ Font.center ]
        --     [ Element.text "You might also be interested "
        --     , newTabLink [] "https://www.youtube.com/watch?v=rH7mjNDD448" "the recording of a workshop"
        --     , Element.text " I ran in 2021 on Bitcoin and crypto."
        --     ]
        , Element.column
            [ Element.spacing 15
            , Element.width Element.fill
            ]
            [ currentWorkTitleEl dProfile
            , portfolioEntryEl dProfile
                (Just "zaptrails-bg.png")
                (Element.el [ Font.size 38 ] <| Element.text "Zap Trails")
                ""
                "research / experimental"
                [ "Recently I discovered the Nostr network, a decentralized social media platform. In practice it's something like Twitter or Medium, depending on the client you use, but without any centralized moderation or control. One of the features of this network is \"zapping\" users for content, which is to send a Lightning Bitcoin payment as a sort of financial upvote."
                , "I'm interested in treating the record of such zaps as a directed graph of socially signaled value. I believe that a set of fairly simple and elegant algorithms is a silver bullet for content curation on Nostr, and further, that a community that primarily uses the zap network to curate content would begin to show some extremely positive and interesting dynamics: valuable content = viral content = profitable content (for the creator)."
                , "My first goal is to demonstrate the basic utility of one of these algorithms, via a tool or visualizations; then I hope to get some traction from the Nostr community."
                ]
                [ newTabLink [] "https://habla.news/a/naddr1qvzqqqr4gupzqyhjp3nd83hxklumz9elp6gmth2zrhr804hrcrktpmplygwtw4jjqqxnzde38q6rwwph8qcrvdpjwz7qav" "writeup on Nostr" ]
            ]
        ]


ytVidEl : DisplayProfile -> Element Msg
ytVidEl dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 15
        ]
        [ Element.paragraph [ Font.size 18 ]
            [ Element.text "For vibe-checking purposes, here's a recording of a workshop I ran on crypto in 2021." ]
        , Element.el
            [ Element.centerX ]
          <|
            Element.html <|
                Embed.Youtube.toHtml <|
                    Embed.Youtube.attributes
                        [ Embed.Youtube.Attributes.width 600
                        , Embed.Youtube.Attributes.height 335

                        -- , Embed.Youtube.Attributes.start
                        ]
                        (Embed.Youtube.fromString "rH7mjNDD448")
        ]


currentWorkTitleEl : DisplayProfile -> Element Msg
currentWorkTitleEl dProfile =
    Element.el
        [ Font.size 44
        ]
        (Element.text "Current Work")


daihardLogoEl : Element Msg
daihardLogoEl =
    Element.row
        [ Font.size 38
        , Font.bold
        ]
        [ Element.el [ Font.color <| Element.rgb 1 1 1 ] <| Element.text "DAI"
        , Element.el [ Font.color <| Element.rgb255 255 0 110 ] <| Element.text "Hard"
        ]


portfolioEntryEl : DisplayProfile -> Maybe String -> Element Msg -> String -> String -> List String -> List (Element Msg) -> Element Msg
portfolioEntryEl dProfile maybeBGImgSrc titleEl dateString roleString bodyStrings linkOutEls =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding 20
        , Border.width 1
        , Border.rounded 8
        , Border.color Theme.portfolioEntryBorderColor
        , case maybeBGImgSrc of
            Just imgSrc ->
                Background.image imgSrc

            Nothing ->
                Background.color Theme.portfolioEntryBackgroundColor
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ titleEl
            , Element.column
                [ Element.alignRight
                , Element.centerY
                ]
                [ Element.el [ Font.size 28, Element.alignRight ] <| Element.text dateString
                , Element.el [ Font.size 18, Element.alignRight ] <| Element.text roleString
                ]
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 15
            , Font.size 18
            ]
            (bodyStrings
                |> List.map
                    (\bodyString ->
                        Element.paragraph
                            [ Element.paddingXY 30 0 ]
                            [ Element.text bodyString ]
                    )
            )
        , Element.row
            [ Element.spacing 30
            , Font.size 16
            ]
            linkOutEls
        ]


tabElement : DisplayProfile -> String -> Msg -> Element Msg
tabElement dProfile label onPress =
    Input.button
        [ Font.size 36 ]
        { onPress = Just onPress
        , label = Element.text label
        }
