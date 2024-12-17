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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Embed.Youtube
import Embed.Youtube.Attributes
import Fonts
import Html
import Html.Attributes
import NymDemo.Config
import NymDemo.Types as NymDemo
import NymDemo.View as NymDemo
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
                            contactModalEl (Responsive.viewportToDisplayProfile loadedModel.viewport) (getScrollbarWidthOrZero loadedModel.viewport loadedModel.bodyViewport)

                        else
                            Element.none
                    ]
                  <|
                    view loadedModel
                ]
    }


getScrollbarWidthOrZero : Viewport -> Maybe Viewport -> Float
getScrollbarWidthOrZero pageViewport maybeBodyViewport =
    Maybe.map
        (\bodyViewport ->
            pageViewport.scene.width - bodyViewport.scene.width
        )
        maybeBodyViewport
        |> Maybe.withDefault 0


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
        , Element.paddingXY (responsiveVal dProfile 20 70) 0
        , Element.clipY
        ]
        [ Element.el
            [ Font.size <| responsiveVal dProfile 35 45
            , Fonts.poppins
            , Font.extraBold
            , Element.moveDown <| headerNameVeritcalDisplacement dProfile model.bodyViewport
            ]
            (nameElement (responsiveVal dProfile "LB" "Logan Brutsche"))
        , Element.row
            [ Element.alignRight
            , Element.spacing <| responsiveVal dProfile 6 53
            , Element.centerY
            , Font.size <| responsiveVal dProfile 10 20
            , Fonts.poppins
            , Font.bold
            ]
            [ routingButton dProfile (Element.text "PROJECTS") (Route.Projects Nothing) model.route
            , routingButton dProfile (Element.text "ABOUT") Route.About model.route
            , Input.button
                [ Element.paddingXY 5 15
                , animatedButtonClass
                ]
                { label =
                    Element.el
                        []
                        (blueBorderedText dProfile "CONTACT")
                , onPress = Just <| SetShowContactModal (not model.showContactModal)
                }
            , Element.newTabLink
                [ Element.paddingXY 5 15
                , animatedButtonClass
                ]
                { url = "/LoganBrutsche_resume.pdf"
                , label =
                    Element.row
                        [ Element.spacing <| responsiveVal dProfile 5 10
                        , Font.color Theme.lightBlue
                        ]
                        [ Element.text "RESUME"
                        , Element.image
                            [ Element.height <| Element.px <| responsiveVal dProfile 11 22
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
        [ Element.paddingXY 5 15
        , animatedButtonClass
        ]
        { label =
            Element.el
                (if route == currentRoute then
                    [ Font.extraBold
                    , Font.color <| Element.rgb 1 1 1
                    , Element.paddingEach
                        { bottom = 5
                        , top = 5
                        , right = 0
                        , left = 0
                        }
                    , Border.widthEach
                        { bottom = 2
                        , left = 0
                        , right = 0
                        , top = 0
                        }
                    , Border.color <| Element.rgb 1 1 1
                    ]

                 else
                    [ Font.color Theme.lightBlue ]
                )
                labelEl
        , onPress = Just <| GotoRoute route
        }


bodyEl : DisplayProfile -> LoadedModel -> Element Msg
bodyEl dProfile model =
    Element.el
        [ addId "body-element"
        , Element.clipY
        , Element.scrollbarY
        , Html.Attributes.style "min-height" "auto"
            |> Element.htmlAttribute
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.behindContent <|
            case model.brickWall of
                Nothing ->
                    Element.none

                Just brickWall ->
                    BrickWall.Draw.view model.animateTime brickWall
        , Element.paddingEach
            { bottom = 100
            , top = 0
            , right = 0
            , left = 0
            }
        ]
    <|
        Element.el
            [ Element.width Element.fill
            , Element.inFront <|
                if model.showContactModal then
                    modalDarkenedClickableBackdrop

                else
                    Element.none
            ]
        <|
            case model.route of
                Route.Projects _ ->
                    viewProjectsPage dProfile model.nymDemoModel

                Route.About ->
                    viewAboutPage dProfile


modalDarkenedClickableBackdrop : Element Msg
modalDarkenedClickableBackdrop =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color <| Element.rgba 0 0 0 0.6
        , Events.onClick <| SetShowContactModal False
        ]
        Element.none


contactModalEl : DisplayProfile -> Float -> Element Msg
contactModalEl dProfile scrollbarWidth =
    let
        closeButton =
            Input.button
                [ Element.alignRight
                , Element.padding <| responsiveVal dProfile 17 30
                ]
                { label =
                    Element.image
                        [ Element.height <| Element.px <| responsiveVal dProfile 19 39
                        ]
                        { src = "/x.png"
                        , description = "close"
                        }
                , onPress = Just <| SetShowContactModal False
                }
    in
    Element.column
        [ Font.size 40
        , Element.alignTop
        , Element.moveDown <| toFloat <| Config.headerHeight dProfile
        , Element.centerX
        , Element.width <| Element.px <| Config.bodyContentWidth dProfile
        , Font.color <| Element.rgb 1 1 1
        , Element.inFront <|
            Element.el
                [ Element.padding <| responsiveVal dProfile 0 30
                , Element.alignRight
                ]
            <|
                closeButton
        , Element.moveLeft (scrollbarWidth / 2)
        ]
        [ Element.column
            [ Background.color <| Element.rgb255 29 134 161
            , Element.width Element.fill
            , Fonts.poppins
            , Element.spacing <| responsiveVal dProfile 16 29
            , Element.paddingEach
                { left = responsiveVal dProfile 20 80
                , right = responsiveVal dProfile 20 80
                , top = responsiveVal dProfile 36 120
                , bottom = 20
                }
            ]
            [ Element.el
                [ Font.size <| responsiveVal dProfile 25 70
                , Font.bold
                ]
              <|
                Element.text "Get in touch"
            , hbreak (responsiveVal dProfile 2 3) (Element.rgb 1 1 1)
            , Element.el
                [ Font.size <| responsiveVal dProfile 13 30
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
                , bottomLeft = responsiveVal dProfile 20 40
                , bottomRight = responsiveVal dProfile 20 40
                }
            , Element.paddingEach
                { left = responsiveVal dProfile 20 80
                , right = responsiveVal dProfile 20 80
                , top = responsiveVal dProfile 10 40
                , bottom = responsiveVal dProfile 20 90
                }
            , Element.spacing <| responsiveVal dProfile 30 80
            ]
            [ Element.column
                [ Font.size <| responsiveVal dProfile 14 30
                , Font.italic
                , Element.paddingXY 20 0
                ]
                [ Element.text "Rockstar developer"
                , Element.text "looking for a rockstar team"
                , Element.text "and challenging work"
                ]
            , viewContactLinks dProfile
            ]
        ]


viewContactLinks : DisplayProfile -> Element Msg
viewContactLinks dProfile =
    Element.row
        [ Element.spacing <| responsiveVal dProfile 30 70
        ]
        (List.map (viewContactLink dProfile)
            [ ( "telegram.png", "https://t.me/coinoplogan" )
            , ( "github.png", "https://github.com/coinop-logan/" )
            , ( "email.png", "mailto:coinop.logan@gmail.com" )
            , ( "medium.png", "https://medium.com/@coinop.logan" )
            ]
        )


viewContactLink : DisplayProfile -> ( String, String ) -> Element Msg
viewContactLink dProfile ( imgFName, url ) =
    Element.newTabLink
        [ animatedButtonClass ]
        { url = url
        , label =
            Element.image [ Element.height <| Element.px <| responsiveVal dProfile 20 60 ]
                { src = "/sm-icons/" ++ imgFName
                , description = imgFName
                }
        }


viewProjectsPage : DisplayProfile -> NymDemo.Model -> Element Msg
viewProjectsPage dProfile nymDemoModel =
    Element.column
        [ Element.width Element.fill
        , Element.spacing <| responsiveVal dProfile 43 140
        ]
        [ nameAndTitleElement dProfile
        , viewPortfolioElements dProfile nymDemoModel
        , Input.button
            [ Element.centerX
            , Element.padding <| responsiveVal dProfile 10 15
            , animatedButtonClass
            ]
            { onPress = Just <| GotoRoute Route.About
            , label =
                Element.row
                    [ Element.spacing <| responsiveVal dProfile 10 20
                    ]
                    [ Element.el
                        [ Font.size <| responsiveVal dProfile 20 35
                        , Font.color <| Element.rgb255 136 231 255
                        , Fonts.poppins
                        , Font.bold
                        ]
                        (Element.text "Current Work")
                    , Element.image
                        [ Element.height <| Element.px <| responsiveVal dProfile 20 35 ]
                        { src = "/arrow-right.png"
                        , description = "Current Work"
                        }
                    ]
            }
        ]


viewAboutPage : DisplayProfile -> Element Msg
viewAboutPage dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing <| responsiveVal dProfile 43 140
        ]
        [ nameAndTitleElement dProfile
        , Element.column
            [ Element.width Element.fill
            , Element.spacing <| responsiveVal dProfile 15 30
            ]
            [ introVidEl dProfile
            , Element.el [ Element.centerX ] <| viewContactLinks dProfile
            ]
        , Element.column
            [ Element.spacing <| responsiveVal dProfile 30 80
            , Element.centerX
            ]
            [ pageSectionHeader dProfile "From my Colleagues"
            , endorsementElement dProfile
                True
                "chase.jpg"
                "Chase Van Etten"
                "CEO at OPFN"
                (responsiveVal dProfile
                    "Logan is the rare engineer that is both competent with his tools and genuinely cares about business goals. He's an excellent partner throughout product development, challenges assumptions that would be easy to overlook, and is considerate of his teammates."
                    "Logan is the rare engineer that is both competent with his tools and genuinely cares about business goals. He's an excellent partner throughout product development, challenges assumptions that would be easy to overlook, and is considerate of his teammates. Logan knows technology is still the best tool for empowering individuals and takes that responsibility seriously."
                )
            , endorsementElement dProfile
                False
                "schalk.jpg"
                "Schalk Dormehl"
                "Cofounder of FoundryDAO\nCTO of Swiftcom"
                "Logan is excellent at plotting out complex projects, then pushing forward in the execution with a consistent, healthy momentum. He keeps the whole scope of the project in mind, and can fill in any needed gaps in things like UX design and cloud infrastructure."
            , endorsementElement dProfile
                True
                "chris.jpg"
                "Chris Lemmer"
                "CEO at SwiftCom"
                "Logan is a standout software architect, who consistently delivers software that works seamlessly. When needed, he builds custom solutions from the ground up. Aside from his technical skills, what really makes Logan shine is his ability to communicate effectively and work well with others."
            ]
        , Element.column
            [ Element.spacing <| responsiveVal dProfile 30 80
            , Element.centerX
            ]
            [ pageSectionHeader dProfile "Current Projects"
            , portfolioEntryEl
                dProfile
                "eestisse"
                (Element.row
                    [ Element.spacing <| responsiveVal dProfile 5 15 ]
                    [ Element.image
                        [ Element.width <| Element.px <| responsiveVal dProfile 100 220 ]
                        { src = "eestisse-title.png"
                        , description = "eestisse"
                        }
                    , Element.newTabLink
                        [ Element.alignTop ]
                        { url = "https://eestisse.ee/"
                        , label =
                            Element.image [ Element.height <| Element.px <| responsiveVal dProfile 15 25 ]
                                { src = "external-link.png"
                                , description = "external link"
                                }
                        }
                    ]
                )
                [ [ "Lamdera/Elm", "Entrepreneurship" ], [ "UX Design", "Prompt Engineering" ] ]
                Nothing
                (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
                    [ "An LLM-powered tool that explains the counter-intuitive Estonian grammar to English speakers (for example, why \"eestisse\" means \"into Estonia\"). The tool takes English or Estonian text, translates it, and explains word by word how the Estonian is constructed."
                    , "The app is mostly a thin wrapper around the surprisingly high competence of LLMs when providing translations and explanations of the translations. The LLM is primed by providing a few examples of expected responses - both content and JSON structure. The app then decodes this and provides the information in a user-friendly manner, word-by-word, and also keeps a history for the user of their previous translations."
                    , "Future work will add flashcard functionality, to strengthen the product as a learning tool."
                    ]
                )
                [ blueOutlineNewTabLink dProfile "https://eestisse.ee" "eestisse.ee"
                , blueOutlineNewTabLink dProfile "https://github.com/eestisse/eestisse" "github"
                ]

            -- , portfolioEntryEl dProfile
            --     "portfolio"
            --     (Element.el
            --         [ Font.size <| responsiveVal dProfile 20 60
            --         , Font.bold
            --         , Fonts.poppins
            --         , Font.color Theme.lightBlue
            --         ]
            --      <|
            --         Element.text "This Portfolio / Job Search"
            --     )
            --     Nothing
            --     (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
            --         [ "I'm currently looking for a job I can commit to, eager to move on from entrepreneurship and short term contracts to something more reliable. At this point I'm confident in my ability to bring a lot of value to a rockstar team."
            --         , "I decided a polished, unique portfolio would strengthen my application package, and it felt right given my goal of finding that perfect fit. I built this portfolio with Elm - had a lot of fun with the bricks animation in the back. Partway through I hired a visual designer to spice things up - something that in retrospect I should have done first!"
            --         , "With the portfolio essentially finished and the resume sharpened up, I'm looking and asking around for any company that has the right culture fit, is working on challenging tasks, and is looking for a high-value architect/developer like myself."
            --         , "My hope is to find a place where I can work hard and passionately over the long term, where I can show up every day excited to throw my full power and creativity at the problems in front of us."
            --         , "(The visual design for this project was contracted out.)"
            --         ]
            --     )
            --     [ blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/coinop-logan.github.io" "portfolio code repo"
            --     , blueOutlineNewTabLink dProfile "/LoganBrutsche_resume.pdf" "resume"
            --     ]
            -- , portfolioEntryEl dProfile
            --     "zaptrails"
            --     (Element.el
            --         [ Font.size <| responsiveVal dProfile 20 60
            --         , Font.bold
            --         , Fonts.poppins
            --         , Font.color Theme.lightBlue
            --         ]
            --      <|
            --         Element.text "Zap Trails"
            --     )
            --     Nothing
            --     (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
            --         [ "I recently discovered the Nostr network, a decentralized social media platform. In practice it's something like Twitter or Medium, depending on the client you use, but without any centralized moderation or control. A complex challenge with such projects is content curation, and filtering out spam."
            --         , "I'm experimenting with a family of algorithms for content curation on Nostr, which addresses this problem by crawling the network's \"zaps\". Zaps are Bitcoin lightning payments that users on Nostr use as a financial upvote, boosting the signal's content while providing funds to the creator. This \"zap network\" is a latent data goldmine that I hope to show can be the core of an elegant solution to the data curation problem."
            --         , "My first goal is to demonstrate the basic utility of one of these algorithms, via a tool or visualizations; then I hope to get some traction from the Nostr community, and seek grant funding to pursue further application and research in this area."
            --         ]
            --     )
            --     [ blueOutlineNewTabLink dProfile "https://habla.news/a/naddr1qvzqqqr4gupzqyhjp3nd83hxklumz9elp6gmth2zrhr804hrcrktpmplygwtw4jjqqxnzde38q6rwwph8qcrvdpjwz7qav" "writeup on Nostr" ]
            ]
        , Input.button
            [ Element.centerX
            , Element.padding <| responsiveVal dProfile 10 15
            , animatedButtonClass
            ]
            { onPress = Just <| GotoRoute (Route.Projects Nothing)
            , label =
                Element.row
                    [ Element.spacing <| responsiveVal dProfile 10 20
                    ]
                    [ Element.image
                        [ Element.height <| Element.px <| responsiveVal dProfile 20 35 ]
                        { src = "/arrow-left.png"
                        , description = "Past Projects"
                        }
                    , Element.el
                        [ Font.size <| responsiveVal dProfile 20 35
                        , Font.color <| Element.rgb255 136 231 255
                        , Fonts.poppins
                        , Font.bold
                        ]
                        (Element.text "Past Projects")
                    ]
            }
        ]


pageSectionHeader : DisplayProfile -> String -> Element Msg
pageSectionHeader dProfile text =
    Element.el
        [ Font.size <| responsiveVal dProfile 20 43
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
        [ Border.rounded <| responsiveVal dProfile 20 40
        , Border.width 3
        , Border.color <| Element.rgb 1 1 1
        , Element.width <| Element.px <| Config.bodyContentWidth dProfile
        , Element.height <| Element.px <| responsiveVal dProfile 256 375
        , Element.clip
        ]
        ([ Element.image
            [ Element.width <| Element.px <| responsiveVal dProfile 126 375
            , Element.height <| Element.px <| responsiveVal dProfile 256 375
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
            , responsiveVal dProfile (Element.paddingXY 18 33) (Element.paddingXY 55 44)
            , Element.spacing <| responsiveVal dProfile 20 30
            , Element.height <| Element.px <| responsiveVal dProfile 256 375
            , Element.centerY
            ]
            [ Element.column
                [ Element.spacing <| responsiveVal dProfile 3 11
                , Font.color <| Element.rgb255 136 231 255
                , Fonts.poppins
                ]
                [ Element.el
                    [ Font.size <| responsiveVal dProfile 12 30
                    , Font.bold
                    ]
                  <|
                    Element.text nameString
                , hbreak 3 <| Element.rgb255 136 231 255
                , Element.el
                    [ Font.size <| responsiveVal dProfile 12 20
                    ]
                  <|
                    Element.text roleString
                ]
            , Element.paragraph
                [ Font.size <| responsiveVal dProfile 10 20
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
            { top = responsiveVal dProfile 36 135
            , bottom = 0
            , right = 0
            , left = 0
            }
        ]
        [ Element.el
            [ Element.centerX
            , Font.size <| responsiveVal dProfile 35 80
            , Font.extraBold
            ]
          <|
            nameElement "Logan Brutsche"
        , Element.el
            [ Element.centerX
            , Font.size <| responsiveVal dProfile 15 40
            ]
          <|
            Element.text "Full-Stack Software Architect"
        ]


nameElement : String -> Element Msg
nameElement name =
    Element.html <|
        Html.div
            [ Html.Attributes.style "background" "-webkit-linear-gradient(left, #88E7FF, #1D86A1)"
            , Html.Attributes.style "-webkit-background-clip" "text"
            , Html.Attributes.style "-webkit-text-fill-color" "transparent"
            , Html.Attributes.style "line-height" "normal"
            ]
        <|
            [ Html.text name ]


introVidEl =
    htmlEmbeddedVidEl


htmlEmbeddedVidEl : DisplayProfile -> Element Msg
htmlEmbeddedVidEl dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 15
        ]
        [ Element.el
            [ Element.centerX ]
          <|
            Element.html <|
                Html.video
                    [ Html.Attributes.width <| Config.bodyContentWidth dProfile
                    , Html.Attributes.height <| floor <| toFloat (Config.bodyContentWidth dProfile) / 1.8
                    , Html.Attributes.poster "portfolio-intro-preview.png"
                    , Html.Attributes.controls True
                    ]
                    [ Html.source
                        [ Html.Attributes.src "portfolio-intro.mp4"
                        , Html.Attributes.type_ "video/mp4"
                        ]
                        [ Html.text "Your browser does not support the video tag." ]
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
                        [ Embed.Youtube.Attributes.width <| Config.bodyContentWidth dProfile
                        , Embed.Youtube.Attributes.height <| floor <| toFloat (Config.bodyContentWidth dProfile) / 1.8
                        ]
                        (Embed.Youtube.fromString "nS8HTce95NY")
        ]


viewPortfolioElements : DisplayProfile -> NymDemo.Model -> Element Msg
viewPortfolioElements dProfile nymDemoModel =
    let
        itemSpacing =
            responsiveVal dProfile 37 130
    in
    Element.column
        [ Element.spacing itemSpacing
        , Element.width Element.fill
        ]
        [ portfolioEntryEl dProfile
            "coinfight"
            (Element.row
                [ Element.spacing <| responsiveVal dProfile 5 15 ]
                [ Element.image
                    [ Element.width <| Element.px <| responsiveVal dProfile 110 330
                    ]
                    { src = "coinfight-title.png"
                    , description = "coinfight"
                    }
                , Element.newTabLink
                    [ Element.alignTop ]
                    { url = "https://coinfight.io/"
                    , label =
                        Element.image [ Element.height <| Element.px <| responsiveVal dProfile 15 25 ]
                            { src = "external-link.png"
                            , description = "link"
                            }
                    }
                ]
            )
            (responsiveVal dProfile
                [ [ "C++", "Multiplayer Networking" ], [ "Blockchain Integration" ], [ "Game Design", "Entrepreneurship" ] ]
                [ [ "C++", "Multiplayer Networking", "Blockchain Integration" ], [ "Game Design", "Entrepreneurship" ] ]
            )
            (Just ( "2022 / 2023", "Solo Project" ))
            (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
                [ "An RTS game where users fight over stablecoins in-game, on the battlefield. Players must invest real crypto into their units (i.e. $1.50 for a Fighter, $0.50 for a worker); if these units are killed, this investment is dropped onto the battlefield for anyone else to pick up, capture, and withdraw. This is a zero-sum game where the goal is to get more out than you put in. \"Like Poker, but the chips shoot at each other!\""
                , "Coinfight was a successful POC that delivered the experience of fighting over real money in real time, something the crypto/gaming industry has still not delivered commercially. This was largely achieved by implementing the game itself with traditional server/client architecture (specifically, a deterministic lockstep protocol). the blockchain is only used to process withdrawals/deposits - a rare but rewarding approach to web3 gaming."
                ]
            )
            [ blueOutlineNewTabLink dProfile "https://www.youtube.com/watch?v=7tw10KUO1_U" "demo video"
            , blueOutlineNewTabLink dProfile "https://medium.com/p/472636deec57" "dev blog post"
            , blueOutlineNewTabLink dProfile "https://coinfight.io/" "coinfight.io"
            , blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/coinfight" "github"
            ]
        , Element.column
            [ Element.centerX
            , Element.spacing <| responsiveVal dProfile 5 20
            , Element.moveLeft <| responsiveVal dProfile 5 0
            , Element.paddingEach
                { left = responsiveVal dProfile 5 56
                , right = 0
                , bottom = responsiveVal dProfile 5 20
                , top = 0
                }
            , Border.rounded <| responsiveVal dProfile 15 40
            , Border.widthEach
                { left = 3
                , top = 0
                , bottom = 0
                , right = 0
                }
            , Border.color <| Element.rgb 1 0.6 0.3
            ]
            [ Element.column
                [ addId "foundrydao"
                , Font.size <| responsiveVal dProfile 15 30
                , Fonts.poppins
                , Element.moveUp <| responsiveVal dProfile 7 15
                , Element.moveRight <| responsiveVal dProfile 15 0
                , Element.spacing <| responsiveVal dProfile 5 15
                ]
                [ Element.row
                    []
                    [ Element.el
                        [ Font.color <| Element.rgb 1 0.6 0.3 ]
                      <|
                        Element.text "FoundryDAO Projects "
                    , Element.text "- "
                    , newTabLink [] "/LoganBrutsche_resume.pdf" "See resume"
                    ]
                , Element.el
                    [ Font.size <| responsiveVal dProfile 12 26
                    ]
                  <|
                    Element.text "2019 - 2021"
                ]
            , Element.column
                [ Element.spacing itemSpacing ]
                [ portfolioEntryEl dProfile
                    "nyms"
                    (Element.row
                        [ Element.spacing <| responsiveVal dProfile 5 15 ]
                        [ Element.el
                            [ Font.size <| responsiveVal dProfile 20 60
                            , Font.bold
                            , Fonts.poppins
                            , Font.color Theme.lightBlue
                            ]
                          <|
                            Element.text "Nyms"
                        , Element.newTabLink
                            [ Element.alignTop ]
                            { url = "https://team-toast.github.io/nym/"
                            , label =
                                Element.image [ Element.height <| Element.px <| responsiveVal dProfile 15 25 ]
                                    { src = "external-link.png"
                                    , description = "link"
                                    }
                            }
                        ]
                    )
                    [ [ "Elm", "Solidity", "NFTs" ], [ "Generative 3D Modelling" ] ]
                    (Just ( "2021", "Solo Project" ))
                    (Element.el
                        [ Element.width <| Element.px (NymDemo.Config.nymDemoRenderDimensions dProfile |> Tuple.first)
                        , Element.centerX
                        ]
                        (NymDemo.view dProfile nymDemoModel |> Element.map NymDemoMsg)
                        :: (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
                                [ "An experimental NFT/identicon project intended to provide cryprographically secure but visually recognizeable identicons."
                                , "The above are generated on-the-fly from 113 bits of entropy (72 for structure and 41 for color) to produce over one thousand quintillion (1,000,000,000,000,000,000,000,000,000,000,000) visually distinct 3D mammalian faces."
                                , "You could stay on this page for millions of years and refresh as many times as you want, and you won't see a repeated face."
                                ]
                           )
                    )
                    [ blueOutlineNewTabLink dProfile "https://team-toast.github.io/nym/" "more info"
                    , blueOutlineNewTabLink dProfile "https://opensea.io/collection/alpha-nyms" "Alpha Nym NFT set"
                    ]
                , portfolioEntryEl dProfile
                    "smokesignal"
                    (Element.image
                        [ Element.width <| Element.px <| responsiveVal dProfile 120 230 ]
                        { src = "smokesignal-title.svg"
                        , description = "smokesignal"
                        }
                    )
                    (responsiveVal dProfile
                        [ [ "Elm", "Solidity" ], [ "Game Theory Design" ], [ "Tech Lead" ] ]
                        [ [ "Elm", "Solidity" ], [ "Game Theory Design", "Tech Lead" ] ]
                    )
                    (Just ( "2020 / 2021", "Tech Lead" ))
                    (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
                        [ "SmokeSignal was an uncensorable, global chat forum. It offered Reddit-like functionality (nested comments in topical forums) and allowed users to tip each other for posts. The backend was entirely implemented with Ethereum smart contracts (other than supporting architecture for things like SEO and analytics); the frontend was a page hosted on IPFS and used an ENS domain."
                        , "As with DAIHard, below, a major goal of SmokeSignal was to be both radically free (no censorship or moderation) and unkillable (no central organization or nation-state could stop it); hence the motivation for the above architecture."
                        ]
                    )
                    [ blueOutlineNewTabLink dProfile "https://medium.com/daihard-buidlers/introducing-smokesignal-da8f19bc27af" "intro post"
                    , blueOutlineNewTabLink dProfile "https://www.youtube.com/watch?v=pV70Q0wgnnU" "demo video"
                    , blueOutlineNewTabLink dProfile "https://github.com/team-toast/SmokeSignal" "github"
                    ]
                , portfolioEntryEl dProfile
                    "daihard"
                    (Element.el
                        [ Font.size <| responsiveVal dProfile 20 60
                        , Font.bold
                        , Fonts.poppins
                        , Font.color Theme.lightBlue
                        ]
                     <|
                        Element.text "DAIHard"
                    )
                    (responsiveVal dProfile
                        [ [ "Elm", "Solidity" ], [ "Cryptographic Protocol Design" ], [ "Game Theory Design", "Tech Lead" ] ]
                        [ [ "Elm", "Solidity", "Cryptographic Protocol Design" ], [ "Game Theory Design", "Tech Lead" ] ]
                    )
                    (Just ( "2019 / 2020", "Solo Developer" ))
                    (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
                        [ "DAIHard was a crypto/fiat exchange built entirely with Ethereum smart contracts, so that there was no central server anyone could take down. The application was designed to continue to function even in adversarial jurisdictions."
                        , "Traders find and engage with each other by depositing crypto and entering into a \"burnable payment\" contract (described in the game theory link below, and prototyped in the Toastycoin project further down). This contract moved through various stages of the deal and allowed users to punish bad actors by burning any funds they might receive as a result of a scam."
                        , "To enable secure communication between traders without any central server, I designed and implemented a messaging protocol that leverages Ethereum event logs as the transport layer. Each conversation is encrypted using dual-key cryptography, with messages visible but unreadable on the public blockchain. Thus, Ethereum's public ledger was used in lieu of a centralized backend server to facilitate secure, pseudonymous communication between strangers - a necessary feature of any p2p trading marketplace."
                        , "As part of this project, I spent two months in Zimbabwe researching the viability of crypto adoption in the face of a hyperinflated currency. A summary of my findings can be found in the below-linked ZimDai paper \"ZimDai: Blueprint for an Economic Jailbreak\"."
                        ]
                    )
                    [ blueOutlineNewTabLink dProfile "https://github.com/team-toast/DAIHard/blob/master/smart-contracts/DAIHard/contracts/DAIHard.sol" "DAIHard Contract (Solidity)"
                    , blueOutlineNewTabLink dProfile "https://www.youtube.com/watch?v=WR4WovM0qwg" "demo video"
                    , blueOutlineNewTabLink dProfile "https://medium.com/@coinop.logan/daihard-game-theory-21a456ef224e" "game theory writeup"
                    , blueOutlineNewTabLink dProfile "https://github.com/team-toast/DAIHard" "github"
                    , blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/ZimDai/blob/master/whitepaper.pdf" "ZimDai paper"
                    ]
                ]
            ]
        , portfolioEntryEl dProfile
            "toastycoin"
            (Element.el
                [ Font.size <| responsiveVal dProfile 20 60
                , Font.bold
                , Fonts.poppins
                , Font.color Theme.lightBlue
                ]
             <|
                Element.text "Toastycoin"
            )
            (responsiveVal dProfile
                [ [ "Javascript", "Solidity" ], [ "Game Theory Design" ], [ "Research/Prototyping" ] ]
                [ [ "Javascript", "Solidity" ], [ "Game Theory Design", "Research/Prototyping" ] ]
            )
            (Just ( "2017", "Solo Project" ))
            (List.map (Element.text >> List.singleton >> Element.paragraph []) <|
                [ "Toastycoin was an experimental dapp that used \"burnable payment\" contracts on the Ethereum blockchain to allow users to contract work from strangers on the Internet, without any previous trust or association."
                , "The burnable payment contracts used game theory to facilitate this, centering around the payer's ability to burn the payment (but not get it back) if the worker does not deliver the requested service. Scammers attempting to game the system would be punished more than they profited in nearly every case - all without any moderation/dispute teams, arbitrators, or escrow system."
                , "See the \"game theory writeup\" link under the DAIHard project above, to read more about this game theory, as DAIHard was simply a narrowed use-case of the burnable payments developed for Toastycoin."
                ]
            )
            [ blueOutlineNewTabLink dProfile "https://medium.com/@coinop.logan/preventing-scammer-profit-with-burnable-payments-ad2e9b632ef2" "Burnable Payments?"
            , blueOutlineNewTabLink dProfile "https://medium.com/@coinop.logan/toasted-money-part-2-b5dfd0b1e946" "experiment conclusion"
            , blueOutlineNewTabLink dProfile "https://github.com/coinop-logan/toastycoin" "github"
            ]
        ]


blueOutlineNewTabLink : DisplayProfile -> String -> String -> Element Msg
blueOutlineNewTabLink dProfile url labelText =
    Element.newTabLink
        [ animatedButtonClass ]
        { url = url
        , label =
            Element.el
                [ Element.padding <| responsiveVal dProfile 5 15
                , Font.color Theme.lightBlue
                , Fonts.poppins
                , Font.bold
                , Font.size <| responsiveVal dProfile 12 25
                , Border.rounded 100
                , Border.width 3
                , Border.color Theme.lightBlue
                ]
                (Element.text labelText)
        }


blueBorderedText : DisplayProfile -> String -> Element Msg
blueBorderedText dProfile text =
    Element.el
        [ Element.padding <| responsiveVal dProfile 4 15
        , Font.color Theme.lightBlue
        , Border.rounded 100
        , Border.width 2
        , Border.color Theme.lightBlue
        ]
        (Element.text text)


portfolioEntryEl : DisplayProfile -> String -> Element Msg -> List (List String) -> Maybe ( String, String ) -> List (Element Msg) -> List (Element Msg) -> Element Msg
portfolioEntryEl dProfile idStr titleEl skillsRows maybeDateAndRoleString bodyEls linkOutEls =
    Element.column
        [ addId idStr
        , Background.color <| Element.rgba255 217 217 217 0.2
        , Border.rounded <| responsiveVal dProfile 20 40
        , responsiveVal dProfile
            (Element.paddingXY 24 40)
            (Element.paddingXY 60 43)
        , Element.spacing <| responsiveVal dProfile 30 80
        , Element.width <| Element.px (Config.bodyContentWidth dProfile)
        , Element.centerX
        ]
        [ projectHeaderEl dProfile titleEl skillsRows maybeDateAndRoleString
        , Element.column
            [ Element.width Element.fill
            , Element.spacing <| responsiveVal dProfile 40 50
            ]
            [ projectBodyEl dProfile bodyEls
            , Element.wrappedRow
                [ Element.spacing <| responsiveVal dProfile 7 26 ]
                linkOutEls
            ]
        ]


viewSkill : DisplayProfile -> String -> Element Msg
viewSkill dProfile skillString =
    Element.el
        [ Element.padding <| responsiveVal dProfile 4 8
        , Border.color <| Theme.lightBlue
        , Background.color <| Element.rgba 0.8 0.8 1 0.2
        , Font.italic
        , Font.extraLight
        , Font.color <| Element.rgb 0.9 0.9 0.9
        , Font.size <| responsiveVal dProfile 10 16
        ]
    <|
        Element.text skillString


projectHeaderEl : DisplayProfile -> Element Msg -> List (List String) -> Maybe ( String, String ) -> Element Msg
projectHeaderEl dProfile titleEl skillsLists maybeDateAndRoleString =
    Element.column
        [ Element.spacing <| responsiveVal dProfile 21 30
        , Element.width Element.fill
        ]
        [ Element.row
            [ Element.width Element.fill
            ]
            [ titleEl
            , Element.column
                [ Element.spacing <| responsiveVal dProfile 7 16
                , Element.alignRight
                ]
                (skillsLists
                    |> List.map
                        (\skillsRow ->
                            Element.row
                                [ Element.spacing <| responsiveVal dProfile 7 16
                                , Element.alignRight
                                ]
                                (List.map (viewSkill dProfile) skillsRow)
                        )
                )
            ]
        , hbreak 3 <| Element.rgb 1 1 1
        , case maybeDateAndRoleString of
            Just ( dateString, roleString ) ->
                Element.row
                    [ Element.width Element.fill
                    , Fonts.poppins
                    , Font.size <| responsiveVal dProfile 15 25
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


projectBodyEl : DisplayProfile -> List (Element Msg) -> Element Msg
projectBodyEl dProfile bodyEls =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 15
        , Font.size <| responsiveVal dProfile 12 25
        , Font.light
        , Fonts.poppins
        ]
        bodyEls


animatedButtonClass : Attribute Msg
animatedButtonClass =
    Element.htmlAttribute <| Html.Attributes.class "animatedButton"
