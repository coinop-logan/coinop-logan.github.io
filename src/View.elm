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
import Fonts
import Html
import Html.Attributes
import Responsive exposing (..)
import Route exposing (Route)
import TabGraphics
import Theme
import Time
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    { title = "Logan Brutsche - Portfolio"
    , body =
        case model of
            Loading _ ->
                [ Element.layout
                    []
                    viewLoadingMessage
                ]

            Loaded loadedModel ->
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
                    , Element.inFront <|
                        if loadedModel.showContactModal then
                            contactModalEl (Responsive.viewportToDisplayProfile loadedModel.viewport)

                        else
                            Element.none
                    ]
                  <|
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
            Responsive.viewportToDisplayProfile model.viewport
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.color <| Element.rgb 1 1 1
        , Background.color Theme.darkNavyBlue
        ]
        [ headerEl dProfile model
        , bodyEl dProfile model
        ]


headerEl : DisplayProfile -> LoadedModel -> Element Msg
headerEl dProfile model =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px <| Config.headerHeight dProfile
        , Background.color Theme.darkNavyBlue
        , Element.paddingXY 70 0
        , Element.clipY
        ]
        [ Element.el
            [ Font.size 45
            , Fonts.poppins
            , Font.extraBold
            , Element.moveDown <| headerNameVeritcalDisplacement dProfile model.bodyViewport
            ]
            nameElement
        , Element.row
            [ Element.alignRight
            , Element.spacing 53
            , Element.centerY
            , Font.size 20
            , Fonts.poppins
            , Font.bold
            ]
            [ routingButton dProfile (Element.text "PROJECTS") Route.Projects model.route
            , routingButton dProfile (Element.text "ABOUT") Route.About model.route
            , Input.button
                []
                { label =
                    Element.el
                        []
                        (blueBorderedText dProfile "CONTACT")
                , onPress = Just <| SetShowContactModal (not model.showContactModal)
                }
            , Element.newTabLink
                []
                { url = "/resume.pdf"
                , label =
                    Element.row
                        [ Element.spacing 10 ]
                        [ Element.text "RESUME"
                        , Element.image
                            [ Element.height <| Element.px 22
                            ]
                            { src = "/download.png"
                            , description = "download"
                            }
                        ]
                }
            ]
        ]


headerNameVeritcalDisplacement : DisplayProfile -> Maybe Viewport -> Float
headerNameVeritcalDisplacement dProfile maybeBodyViewport =
    case maybeBodyViewport of
        Just bodyViewport ->
            let
                scrolledThroughNameFloat =
                    (bodyViewport.viewport.y - 200) / 120.0
            in
            70
                - (scrolledThroughNameFloat * 60)
                |> max 0

        Nothing ->
            100


routingButton : DisplayProfile -> Element Msg -> Route -> Route -> Element Msg
routingButton dProfile labelEl route currentRoute =
    Input.button
        [ Element.padding 10 ]
        { label =
            Element.el
                (if route == currentRoute then
                    [ Font.extraBold
                    ]

                 else
                    []
                )
                labelEl
        , onPress = Just <| GotoRoute route
        }


bodyEl : DisplayProfile -> LoadedModel -> Element Msg
bodyEl dProfile model =
    Element.el
        [ Element.clipY
        , Element.scrollbarX
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.behindContent <|
            case model.bodyViewport of
                Nothing ->
                    Element.none

                Just bodyViewport ->
                    BrickWall.Draw.view model.animateTime bodyViewport.scene.width bodyViewport.scene.height model.brickWall
        , addId "body-element"
        , Element.paddingEach
            { bottom = 100
            , top = 0
            , right = 0
            , left = 0
            }
        ]
    <|
        case model.route of
            Route.Projects ->
                viewProjectsPage dProfile

            Route.About ->
                viewAboutPage dProfile


contactModalEl : DisplayProfile -> Element Msg
contactModalEl dProfile =
    Element.column
        [ Font.size 40
        , Element.alignTop
        , Element.moveDown <| toFloat <| Config.headerHeight dProfile
        , Element.centerX
        , Element.width <| Element.px <| Config.bodyContentWidth dProfile
        , Font.color <| Element.rgb 1 1 1
        ]
        [ Element.column
            [ Background.color <| Element.rgb255 29 134 161
            , Element.width Element.fill
            , Fonts.poppins
            , Element.spacing 29
            , Element.paddingEach
                { left = 80
                , right = 80
                , top = 54
                , bottom = 20
                }
            ]
            [ Input.button
                [ Element.alignRight
                ]
                { label =
                    Element.image
                        [ Element.height <| Element.px 39
                        ]
                        { src = "/x.png"
                        , description = "close"
                        }
                , onPress = Just <| SetShowContactModal False
                }
            , Element.el
                [ Font.size 70
                , Font.bold
                ]
              <|
                Element.text "Get in touch"
            , hbreak <| Element.rgb 1 1 1
            , Element.el
                [ Font.size 30
                , Font.bold
                ]
              <|
                Element.text "coinop.logan@gmail.com"
            ]
        , Element.column
            [ Background.gradient
                { angle = pi
                , steps =
                    [ Element.rgb255 29 134 161
                    , Element.rgb255 100 200 220
                    ]
                }
            , Element.width Element.fill
            , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = 40
                , bottomRight = 40
                }
            , Element.paddingEach
                { left = 80
                , right = 80
                , top = 40
                , bottom = 90
                }
            , Element.spacing 80
            ]
            [ Element.column
                [ Font.size 30
                , Font.italic
                , Element.paddingXY 20 0
                ]
                [ Element.text "Rockstar developer"
                , Element.text "looing for a rockstar team"
                , Element.text "and challenging work"
                ]
            , Element.row
                [ Element.spacing <| responsiveVal dProfile 30 50
                ]
                (List.map (viewContactLink dProfile)
                    [ ( "github.png", "https://github.com/coinop-logan/" )
                    , ( "telegram.png", "https://t.me/coinoplogan" )
                    , ( "medium.png", "https://medium.com/@coinop.logan" )
                    ]
                )
            ]
        ]


viewContactLink : DisplayProfile -> ( String, String ) -> Element Msg
viewContactLink dProfile ( imgFName, url ) =
    Element.newTabLink
        []
        { url = url
        , label =
            Element.image [ Element.height <| Element.px <| responsiveVal dProfile 55 85 ]
                { src = "/sm-icons/" ++ imgFName
                , description = imgFName
                }
        }


viewProjectsPage : DisplayProfile -> Element Msg
viewProjectsPage dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 140
        ]
        [ nameAndTitleElement dProfile
        , viewPortfolioElements dProfile
        ]


viewAboutPage : DisplayProfile -> Element Msg
viewAboutPage dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 140
        ]
        [ nameAndTitleElement dProfile
        , ytVidEl dProfile
        , Element.column
            [ Element.spacing 80
            , Element.centerX
            ]
            [ pageSectionHeader "Testimonials"
            , endorsementElement dProfile
                True
                "chase.jpg"
                "Chase Van Etten"
                "CEO at OPFN"
                "Logan is the rare engineer that is both competent with his tools and genuinely cares about business goals. He's an excellent partner throughout product development, challenges assumptions that would be easy to overlook, and is considerate of his teammates. Logan knows technology is still the best tool for empowering individuals and takes that responsibility seriously."
            , endorsementElement dProfile
                False
                "schalk.jpg"
                "Schalk Dormehl"
                "CTO of Swiftcom"
                "Logan is excellent at plotting out complex projects, then pushing forward in the execution with a consistent, healthy momentum. He keeps the whole scope of the project in mind, and can fill in any needed gaps in things like UX design and cloud infrastructure."
            , endorsementElement dProfile
                True
                "chris.jpg"
                "Chris Lemmer"
                "CEO at SwiftCom"
                "Logan is a standout software architect,  who consistently delivered collaborative software that worked seamlessly. When faced with challenges, he didn't hesitate to build custom solutions from the ground up. His technical skills are top-notch, but what really makes Logan shine is his ability to communicate effectively and work well with others."
            ]
        , Element.column
            [ Element.spacing 80
            , Element.centerX
            ]
            [ pageSectionHeader "Current Projects"
            , portfolioEntryEl dProfile
                (Element.el [ Font.size <| responsiveVal dProfile 30 38 ] <| Element.text "Zap Trails")
                Nothing
                (responsiveVal dProfile
                    [ "The Nostr network allows users to send \"zaps\" (Lightning Bitcoin) to one another; I see the record of such zaps as quite a data goldmine - a directed graph of socially signaled value. I'm experimenting using this to curate content and route around spam with a family of simple algorithms that I expect to yield extremely impressive results."
                    , "My first goal is to demonstrate the basic utility of one of these algorithms, via a tool or visualizations; then I hope to get some traction from the Nostr community, and seek grant funding to pursue further application and research in this area."
                    ]
                    [ "I recently discovered the Nostr network, a decentralized social media platform. In practice it's something like Twitter or Medium, depending on the client you use, but without any centralized moderation or control. One of the features of this network is \"zapping\" users for content, which is to send a Lightning Bitcoin payment as a financial upvote."
                    , "I see the the record of such zaps as quite a data goldmine - a directed graph of socially signaled value. I'm experimenting with a family of fairly simple algorithms for content curation on Nostr, and hope to prove that they elegantly solve data curation problems, finding valuable new content and routing around spam - an otherwise tricky problem in a sea of unmoderated content and pseudonymous accounts."
                    , "In addition, I believe that a community primarily using such a technique to construct feeds would begin to behave like something of a neural net, with users as neurons and zaps as synapse firings, forming connections and propagating content further through the network. I discuss this further in the article linked below."
                    , "My first goal is to demonstrate the basic utility of one of these algorithms, via a tool or visualizations; then I hope to get some traction from the Nostr community, and seek grant funding to pursue further application and research in this area."
                    ]
                )
                [ blueOutlineNewTabLink dProfile "https://habla.news/a/naddr1qvzqqqr4gupzqyhjp3nd83hxklumz9elp6gmth2zrhr804hrcrktpmplygwtw4jjqqxnzde38q6rwwph8qcrvdpjwz7qav" "writeup on Nostr" ]
            ]
        ]


pageSectionHeader : String -> Element Msg
pageSectionHeader text =
    Element.el
        [ Font.size 43
        , Font.color <| Element.rgb255 136 231 255
        , Font.extraBold
        , Fonts.poppins
        , Element.centerX
        ]
        (Element.text text)


endorsementElement : DisplayProfile -> Bool -> String -> String -> String -> String -> Element Msg
endorsementElement dProfile isReversed picSrc nameString roleString quoteString =
    let
        picPath =
            responsiveVal dProfile "endorsements/mobile/" "endorsements/desktop/"
    in
    Element.row
        [ Border.rounded 40
        , Border.width 3
        , Border.color <| Element.rgb 1 1 1
        , Element.width <| Element.px <| Config.bodyContentWidth dProfile
        , Element.height <| Element.px <| 375
        , Element.clip
        ]
        ([ Element.image
            [ Element.height Element.fill
            ]
            { src = picPath ++ picSrc
            , description = nameString
            }
         , Element.el
            [ Element.height Element.fill
            , Element.width <| Element.px 3
            , Background.color <| Element.rgb 1 1 1
            ]
            Element.none
         , Element.column
            [ Element.width Element.fill
            , Element.paddingXY 72 56
            , Element.spacing 41
            ]
            [ Element.column
                [ Element.spacing 11
                , Font.color <| Element.rgb255 136 231 255
                , Fonts.poppins
                ]
                [ Element.el
                    [ Font.size 30
                    , Font.bold
                    ]
                  <|
                    Element.text nameString
                , hbreak <| Element.rgb255 136 231 255
                , Element.el
                    [ Font.size 20
                    ]
                  <|
                    Element.text roleString
                ]
            , Element.paragraph
                [ Font.size 20
                , Fonts.poppins
                ]
                [ Element.text quoteString ]
            ]
         ]
            |> (if isReversed then
                    List.reverse

                else
                    identity
               )
        )


nameAndTitleElement : DisplayProfile -> Element Msg
nameAndTitleElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 0
        , Fonts.poppins
        , Element.paddingEach
            { top = 135
            , bottom = 0
            , right = 0
            , left = 0
            }
        ]
        [ Element.el
            [ Element.centerX
            , Font.size 80
            , Font.extraBold
            ]
          <|
            nameElement
        , Element.el
            [ Element.centerX
            , Font.size 40
            ]
          <|
            Element.text "Full-Stack Software Architect"
        ]


nameElement : Element Msg
nameElement =
    Element.html <|
        Html.div
            [ Html.Attributes.style "background" "-webkit-linear-gradient(left, #88E7FF, #1D86A1)"
            , Html.Attributes.style "-webkit-background-clip" "text"
            , Html.Attributes.style "-webkit-text-fill-color" "transparent"
            , Html.Attributes.style "line-height" "normal"
            ]
        <|
            [ Html.text "Logan Brutsche" ]


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
                        [ Embed.Youtube.Attributes.width <| Config.bodyContentWidth dProfile
                        , Embed.Youtube.Attributes.height <| floor <| toFloat (Config.bodyContentWidth dProfile) / 1.8
                        ]
                        (Embed.Youtube.fromString "nS8HTce95NY")
        ]


viewPortfolioElements : DisplayProfile -> Element Msg
viewPortfolioElements dProfile =
    let
        itemSpacing =
            130
    in
    Element.column
        [ Element.spacing itemSpacing
        , Element.width Element.fill
        ]
        [ portfolioEntryEl dProfile
            (Element.image
                [ Element.width <| Element.px 220 ]
                { src = "eestisse-title.png"
                , description = "eestisse"
                }
            )
            (Just ( "2024", "Solo Project" ))
            [ "An LLM-powered tool that explains the counter-intuitive Estonian grammar to English speakers (for example, why \"eestisse\" means \"into Estonia\"). The tool takes English or Estonian text, translates it, and explains word by word how the Estonian is constructed."
            , "The central feature was surprisingly easy to build, due to LLM's strength in language tasks."
            ]
            [ blueOutlineNewTabLink dProfile "https://eestisse.ee" "eestisse.ee"
            , blueOutlineNewTabLink dProfile "https://github.com/eestisse/eestisse" "github"
            ]
        , portfolioEntryEl dProfile
            (Element.image
                [ Element.width <| Element.px 330
                ]
                { src = "coinfight-title.png"
                , description = "coinfight"
                }
            )
            (Just ( "2022 / 2023", "Solo Project" ))
            [ "An RTS game where users fight over crypto in-game. Players must invest real crypto into their units (i.e. $1.50 for a Fighter, $0.50 for a worker); if these units are killed, this investment is dropped onto the battlefield for anyone else to pick up, capture, and withdraw. This is a zero-sum game where the goal is to get more out than you put in. \"Like Poker, but the chips shoot at each other!\""
            , "The goal of Coinfight was to give players the experience of fighting over real money in real time. To avoid the cumbersome limits of blockchain processing, Coinfight only used the blockchain to process deposits and withdrawals, a rare but rewarding architectural approach among web3 games."
            ]
            [ blueOutlineNewTabLink dProfile "https://www.youtube.com/watch?v=7tw10KUO1_U" "demo video"
            , blueOutlineNewTabLink dProfile "https://medium.com/p/472636deec57" "dev blog post"
            , blueOutlineNewTabLink dProfile "https://coinfight.io/" "coinfight.io"
            , blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/coinfight" "github"
            ]
        , Element.column
            [ Element.centerX
            , Element.spacing 20
            , Element.paddingEach
                { left = 56
                , right = 0
                , bottom = 20
                , top = 0
                }
            , Border.rounded 40
            , Border.widthEach
                { left = 3
                , top = 0
                , bottom = 0
                , right = 0
                }
            , Border.color <| Element.rgb 1 0.6 0.3
            ]
            [ Element.row
                [ Font.size 30
                , Font.color <| Element.rgb 1 0.6 0.3
                , Fonts.poppins
                , Element.moveUp 15
                ]
                [ Element.text "FoundryDAO Projects - "
                , newTabLink [] "/resume.pdf" "See resume"
                ]
            , Element.column
                [ Element.spacing itemSpacing ]
                [ portfolioEntryEl dProfile
                    (Element.el
                        [ Font.size 60
                        , Font.bold
                        , Fonts.poppins
                        , Font.color Theme.lightBlue
                        ]
                     <|
                        Element.text "Nyms"
                    )
                    (Just ( "2021", "Solo Project" ))
                    [ "TODO" ]
                    [ blueOutlineNewTabLink dProfile "https://team-toast.github.io/nym/" "home page" ]
                , portfolioEntryEl dProfile
                    (Element.image
                        [ Element.width <| Element.px 230 ]
                        { src = "smokesignal-title.svg"
                        , description = "smokesignal"
                        }
                    )
                    (Just ( "2020 / 2021", "Tech Lead" ))
                    [ "SmokeSignal was an uncensorable, global chat forum. It implemented Reddit-like functionality (nested comments in topical forums) and allowed users to tip each other for posts."
                    , "As with DAIHard, below, a major goal of SmokeSignal was to be both radically free (no censorship or moderation) and unkillable (no central organization or nation-state could stop it)."
                    , "Thus, the main technical challenge was in making something suitably decentralized so as to not be attackable, while still integrating with traditional frameworks and services for the purposes of marketing and usability. For example, while all core functionality was implemented on the Ethereum blockchain and an interface hosted on IPFS, a traditional web server was used to serve SEO information for the otherwise decentralized content."
                    ]
                    [ blueOutlineNewTabLink dProfile "https://medium.com/daihard-buidlers/introducing-smokesignal-da8f19bc27af" "intro post"
                    , blueOutlineNewTabLink dProfile "https://www.youtube.com/watch?v=pV70Q0wgnnU" "demo video"
                    , blueOutlineNewTabLink dProfile "https://github.com/team-toast/SmokeSignal" "github"
                    ]
                , portfolioEntryEl dProfile
                    (Element.el
                        [ Font.size 60
                        , Font.bold
                        , Fonts.poppins
                        , Font.color Theme.lightBlue
                        ]
                     <|
                        Element.text "DAIHard"
                    )
                    (Just ( "2019 / 2020", "Solo Developer" ))
                    [ "DAIHard was a crypto/fiat exchange built entirely with Ethereum smart contracts, so that there was no central server anyone could take down. The application was designed to continue to function even in adversarial jurisdictions. Note that this app used no backend server at all, even for encrypted chat between users."
                    , "As part of this project, I spent two months in Zimbabwe researching the viability of crypto adoption in the face of a hyperinflated currency. A summary of my findings can be found in the below-linked ZimDai paper \"ZimDai: Blueprint for an Economic Jailbreak\"."
                    , "(The visual design for this project was contracted out.)"
                    ]
                    [ blueOutlineNewTabLink dProfile "https://www.youtube.com/watch?v=WR4WovM0qwg" "demo video"
                    , blueOutlineNewTabLink dProfile "https://medium.com/@coinop.logan/daihard-game-theory-21a456ef224e" "game theory writeup"
                    , blueOutlineNewTabLink dProfile "https://github.com/team-toast/DAIHard" "github"
                    , blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/ZimDai/blob/master/whitepaper.pdf" "ZimDai paper"
                    ]
                ]
            ]
        , portfolioEntryEl dProfile
            (Element.el
                [ Font.size 60
                , Font.bold
                , Fonts.poppins
                , Font.color Theme.lightBlue
                ]
             <|
                Element.text "Toastycoin"
            )
            (Just ( "2017", "Solo Project" ))
            [ "Toastycoin was an experimental dapp that used \"burnable payment\" contracts on the Ethereum blockchain to allow users to contract work from strangers on the Internet, without any previous trust or association. The burnable payment contracts used game theory to facilitate this: while loss of funds was not guaranteed, what was guaranteed was that scammers attempting to game the system would be punished and would not make a profit."
            , "See the \"game theory writeup\" link under the DAIHard project above, to read more about this game theory, as DAIHard was simply a narrowed use-case of the burnable payments developed for Toastycoin."
            ]
            [ blueOutlineNewTabLink dProfile "https://medium.com/@coinop.logan/preventing-scammer-profit-with-burnable-payments-ad2e9b632ef2" "Burnable Payments proposal"
            , blueOutlineNewTabLink dProfile "https://medium.com/@coinop.logan/toasted-money-part-2-b5dfd0b1e946" "experiment conclusion"
            , blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/toastycoin" "github"
            ]
        ]


blueOutlineNewTabLink : DisplayProfile -> String -> String -> Element Msg
blueOutlineNewTabLink dProfile url labelText =
    Element.newTabLink
        []
        { url = url
        , label =
            Element.el
                [ Element.padding 15
                , Font.color Theme.lightBlue
                , Fonts.poppins
                , Font.bold
                , Font.size 25
                , Border.rounded 100
                , Border.width 3
                , Border.color Theme.lightBlue
                ]
                (Element.text labelText)
        }


viewContactPage : DisplayProfile -> Element Msg
viewContactPage dProfile =
    Element.text "viewContactPage"


blueBorderedText : DisplayProfile -> String -> Element Msg
blueBorderedText dProfile text =
    Element.el
        [ Element.padding 15
        , Font.color Theme.lightBlue
        , Border.rounded 100
        , Border.width 2
        , Border.color Theme.lightBlue
        ]
        (Element.text text)


portfolioEntryEl : DisplayProfile -> Element Msg -> Maybe ( String, String ) -> List String -> List (Element Msg) -> Element Msg
portfolioEntryEl dProfile titleEl maybeDateAndRoleString bodyStrings linkOutEls =
    Element.column
        [ Background.color <| Element.rgba255 217 217 217 0.2
        , Border.rounded 40
        , Element.paddingXY 60 43
        , Element.spacing 80
        , Element.width <| Element.px (Config.bodyContentWidth dProfile)
        , Element.centerX
        ]
        [ projectHeaderEl dProfile titleEl maybeDateAndRoleString
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 50
            ]
            [ projectBodyTextEl dProfile bodyStrings
            , Element.row
                [ Element.spacing 26 ]
                linkOutEls
            ]
        ]


projectHeaderEl : DisplayProfile -> Element Msg -> Maybe ( String, String ) -> Element Msg
projectHeaderEl dProfile titleEl maybeDateAndRoleString =
    Element.column
        [ Element.spacing 30
        , Element.width Element.fill
        ]
        [ titleEl
        , hbreak <| Element.rgb 1 1 1
        , case maybeDateAndRoleString of
            Just ( dateString, roleString ) ->
                Element.row
                    [ Element.width Element.fill
                    , Fonts.poppins
                    , Font.size 25
                    ]
                    [ Element.text dateString
                    , Element.el
                        [ Element.alignRight ]
                      <|
                        Element.text roleString
                    ]

            Nothing ->
                Element.none
        ]


projectBodyTextEl : DisplayProfile -> List String -> Element Msg
projectBodyTextEl dProfile bodyStrings =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 15
        , Font.size 25
        , Font.light
        , Fonts.poppins
        ]
        (bodyStrings
            |> List.map
                (\bodyString ->
                    Element.paragraph
                        []
                        [ Element.text bodyString ]
                )
        )
