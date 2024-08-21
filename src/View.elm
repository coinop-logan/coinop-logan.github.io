module View exposing (..)

import BrickWall.Draw
import Browser
import Browser.Dom exposing (Viewport)
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
import Responsive exposing (..)
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
            , Element.height <| calcNeededBodyHeight model
            , Element.clipY

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


calcNeededBodyHeight : Model -> Element.Length
calcNeededBodyHeight model =
    case model of
        Loading _ ->
            Element.fill

        Loaded loadedModel ->
            let
                dProfile =
                    viewportToDisplayProfile loadedModel.viewport

                tabHeight =
                    case loadedModel.tabState of
                        OnTab PastWork ->
                            Config.pastWorkTabBottmY dProfile

                        OnTab CurrentWork ->
                            Config.currentWorkTabBottomY dProfile

                        _ ->
                            200
            in
            Element.px <| floor <| tabHeight + responsiveVal dProfile 220 250


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
            Responsive.viewportToDisplayProfile model.viewport
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
            , Element.spacing 40
            , Element.padding 30
            , Element.behindContent <|
                BrickWall.Draw.view model.animateTime model.viewport.scene.width model.viewport.scene.height model.brickWall
            ]
            [ headerElement dProfile
            , bodyElement model.viewport model.tabState model.animateTime
            ]


headerElement : DisplayProfile -> Element Msg
headerElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 20
        ]
        [ Element.column
            [ Element.centerX
            , Element.spacing <| responsiveVal dProfile 15 20
            , addId "header-element"
            ]
            [ Element.el
                [ Element.centerX
                , Font.size <| responsiveVal dProfile 40 80
                , Font.family [ Font.typeface "roboto" ]
                , Font.color <| Element.rgb 0.5 0.7 1
                ]
              <|
                Element.text "Logan Brutsche"
            , Element.el
                [ Element.centerX
                , Font.size <| responsiveVal dProfile 23 40
                , Font.family [ Font.typeface "inconsolata" ]
                , Font.color <| Element.rgb 1 1 1
                ]
              <|
                Element.text "Full-Stack Software Architect"
            ]
        ]


bodyElement : Viewport -> TabState -> Time.Posix -> Element Msg
bodyElement viewport tabState animateTime =
    let
        dProfile =
            Responsive.viewportToDisplayProfile viewport

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
            viewport.scene.width

        tabBodyWidth =
            responsiveVal dProfile 300.0 800.0

        tabSeparation =
            responsiveVal dProfile 7 10

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

                        truncatedEasingFunction x =
                            easingFunction (x * Config.easingFunctionTruncateMultiplier)

                        offsetMultiplier =
                            let
                                progressFloat =
                                    animationProgressFloat animateStartTime animateTime
                            in
                            if progressFloat < 0.5 then
                                truncatedEasingFunction (progressFloat * 2)

                            else
                                1 - truncatedEasingFunction ((progressFloat - 0.5) * 2)
                    in
                    offsetMultiplier * (tabBodyWidth / 2)

        pastWorkTabEls =
            let
                tabTopWidth =
                    responsiveVal dProfile 120 240

                tabVerticalAdjustment =
                    if tabOnTop /= PastWork then
                        10

                    else
                        0
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = Config.pastWorkTabBottmY dProfile
                , bodyTopY = responsiveVal dProfile 70 100 + tabVerticalAdjustment
                , tabTopY = 10 + tabVerticalAdjustment
                , fillColor = Theme.portfolioTabBackgroundColor
                , strokeColor = Theme.tabBorderColor
                , pathThickness = responsiveVal dProfile 4 6
                , cornerRadius = responsiveVal dProfile 20 40
                , tabTopStartX = (canvasWidth / 2) - tabTopWidth - (tabSeparation / 2)
                , tabTopEndX = canvasWidth / 2 - (tabSeparation / 2)
                , bodyExtendsLeft = ((tabBodyWidth / 2) - tabTopWidth) + xOffsetAbs - (tabSeparation / 2)
                , bodyExtendsRight = tabBodyWidth / 2 - xOffsetAbs + (tabSeparation / 2)
                , canvasWidth = Element.px <| floor <| canvasWidth
                }
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                 <|
                    tabElement dProfile "Past Work" PortfolioClicked
                )
                (if tabOnTop == PastWork then
                    Just <| pastWorkEl dProfile

                 else
                    Nothing
                )

        currentWorkTabEls =
            let
                tabTopWidth =
                    responsiveVal dProfile 120 240

                tabVerticalAdjustment =
                    if tabOnTop /= CurrentWork then
                        10

                    else
                        0
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = Config.currentWorkTabBottomY dProfile
                , bodyTopY = responsiveVal dProfile 70 100 + tabVerticalAdjustment
                , tabTopY = 10 + tabVerticalAdjustment
                , fillColor = Theme.currentWorkTabBackgroundColor
                , strokeColor = Theme.tabBorderColor
                , pathThickness = responsiveVal dProfile 4 6
                , cornerRadius = responsiveVal dProfile 20 40
                , tabTopStartX = canvasWidth / 2 + (tabSeparation / 2)
                , tabTopEndX = (canvasWidth / 2) + tabTopWidth + (tabSeparation / 2)
                , bodyExtendsLeft = tabBodyWidth / 2 - xOffsetAbs + (tabSeparation / 2)
                , bodyExtendsRight = ((tabBodyWidth / 2) - tabTopWidth) + xOffsetAbs - (tabSeparation / 2)
                , canvasWidth = Element.px <| floor <| canvasWidth
                }
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                 <|
                    tabElement dProfile "About Me" CurrentWorkClicked
                )
                (if tabOnTop == CurrentWork then
                    Just <| aboutMeEl dProfile

                 else
                    Nothing
                )

        elsToStack =
            case tabOnTop of
                CurrentWork ->
                    [ pastWorkTabEls.tabShape

                    -- , portfolioTabEls.bodyEl
                    , currentWorkTabEls.tabShape
                    , currentWorkTabEls.bodyEl
                    , pastWorkTabEls.tabEl
                    , currentWorkTabEls.tabEl
                    ]

                PastWork ->
                    [ currentWorkTabEls.tabShape

                    -- , currentWorkTabEls.bodyEl
                    , pastWorkTabEls.tabShape
                    , pastWorkTabEls.bodyEl
                    , currentWorkTabEls.tabEl
                    , pastWorkTabEls.tabEl
                    ]
    in
    stackElementsInZ [ Element.centerX, Element.height Element.fill ] <| elsToStack


pastWorkEl : DisplayProfile -> Element Msg
pastWorkEl dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 35
        , Element.padding <| responsiveVal dProfile 15 45
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
            , "The central feature was surprisingly easy to build, due to LLM's strength in language tasks."
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
            [ "An RTS game where users fight over crypto in-game. Players must invest real crypto into their units (i.e. $1.50 for a Fighter, $0.50 for a worker); if these units are killed, this investment is dropped onto the battlefield for anyone else to pick up, capture, and withdraw. This is a zero-sum game where the goal is to get more out than you put in. \"Like Poker, but the chips shoot at each other!\""
            , "The goal of Coinfight was to give players the experience of fighting over real money in real time. To avoid the cumbersome limits of blockchain processing, Coinfight only used the blockchain to process deposits and withdrawals, a rare but rewarding architectural approach among web3 games."
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
            [ "SmokeSignal was an uncensorable, global chat forum. It implemented Reddit-like functionality (nested comments in topical forums) and allowed users to tip each other for posts."
            , "As with DAIHard, below, a major goal of SmokeSignal was to be both radically free (no censorship or moderation) and unkillable (no central organization or nation-state could stop it)."
            , "Thus, the main technical challenge was in making something suitably decentralized so as to not be attackable, while still integrating with traditional frameworks and services for the purposes of marketing and usability. For example, while all core functionality was implemented on the Ethereum blockchain and an interface hosted on IPFS, a traditional web server was used to serve SEO information for the otherwise decentralized content."
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
            [ "DAIHard was a crypto/fiat exchange built entirely with Ethereum smart contracts, so that there was no central server anyone could take down. The application was designed to continue to function even in adversarial jurisdictions. Note that this app used no backend server at all, even for encrypted chat between users."
            , "As part of this project, I spent two months in Zimbabwe researching the viability of crypto adoption in the face of a hyperinflated currency. A summary of my findings can be found in the below-linked ZimDai paper \"ZimDai: Blueprint for an Economic Jailbreak\"."
            , "(The visual design for this project was contracted out.)"
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
        , Element.padding <| responsiveVal dProfile 15 45
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
                [ "I recently discovered the Nostr network, a decentralized social media platform. In practice it's something like Twitter or Medium, depending on the client you use, but without any centralized moderation or control. One of the features of this network is \"zapping\" users for content, which is to send a Lightning Bitcoin payment as a financial upvote."
                , "I see the the record of such zaps as quite a data goldmine - a directed graph of socially signaled value. I'm experimenting with a family of fairly simple algorithms for content curation on Nostr, and hope to prove that they elegantly solve data curation problems, finding valuable new content and routing around spam - an otherwise tricky problem in a sea of unmoderated content and pseudonymous accounts."
                , "In addition, I believe that a community primarily using such a technique to construct feeds would begin to behave like something of a neural net, with users as neurons and zaps as synapse firings, forming connections and propagating content further through the network. I discuss this further in the article linked below."
                , "My first goal is to demonstrate the basic utility of one of these algorithms, via a tool or visualizations; then I hope to get some traction from the Nostr community, and possible grant funding to pursue further application and research in this area."
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
        [ Element.el
            [ Element.centerX ]
          <|
            Element.html <|
                Embed.Youtube.toHtml <|
                    Embed.Youtube.attributes
                        [ Embed.Youtube.Attributes.width <| responsiveVal dProfile 270 600
                        , Embed.Youtube.Attributes.height <| responsiveVal dProfile 170 335

                        -- , Embed.Youtube.Attributes.start
                        ]
                        (Embed.Youtube.fromString "nS8HTce95NY")
        ]


currentWorkTitleEl : DisplayProfile -> Element Msg
currentWorkTitleEl dProfile =
    Element.el
        [ Font.size <| responsiveVal dProfile 38 44
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
        , Element.padding <| responsiveVal dProfile 10 20
        , Border.width 1
        , Border.rounded 8
        , Border.color Theme.portfolioEntryBorderColor
        , case maybeBGImgSrc of
            Just imgSrc ->
                Background.image imgSrc

            Nothing ->
                Background.color Theme.portfolioEntryBackgroundColor
        ]
        [ case dProfile of
            Desktop ->
                Element.row
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

            Mobile ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 5
                    ]
                    [ titleEl
                    , Element.row
                        [ Element.width Element.fill ]
                        [ Element.el [ Font.size 24 ] <| Element.text dateString
                        , Element.el [ Font.size 16, Element.alignRight ] <| Element.text roleString
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
                            [ Element.paddingXY (responsiveVal dProfile 5 30) 0 ]
                            [ Element.text bodyString ]
                    )
            )
        , case dProfile of
            Desktop ->
                Element.row
                    [ Element.spacing 30
                    , Font.size 16
                    ]
                    linkOutEls

            Mobile ->
                Element.column
                    [ Element.spacing 4
                    , Font.size 16
                    ]
                    linkOutEls
        ]


tabElement : DisplayProfile -> String -> Msg -> Element Msg
tabElement dProfile label onPress =
    Input.button
        [ Font.size <| responsiveVal dProfile 20 36 ]
        { onPress = Just onPress
        , label = Element.text label
        }
