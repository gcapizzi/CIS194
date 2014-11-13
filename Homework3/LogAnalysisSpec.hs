import Test.Hspec
import Log
import LogAnalysis

main :: IO ()
main = hspec $ do
    describe "LogAnalysis.parseMessage" $ do
        it "parses an Unknown message" $ do
            parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

        it "parses an Info message" $ do
            parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

        it "parses a Warning message" $ do
            parseMessage "W 29 la la la" `shouldBe` LogMessage Warning 29 "la la la"

        it "parses an Error message" $ do
            parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    describe "LogAnalysis.parse" $ do
        it "parses a whole log file" $ do
            let logFile = "This is not in the right format\n\
                           \I 29 la la la\n\
                           \W 29 la la la\n\
                           \E 2 562 help help"
            parse logFile `shouldBe` [Unknown "This is not in the right format",
                                      LogMessage Info 29 "la la la",
                                      LogMessage Warning 29 "la la la",
                                      LogMessage (Error 2) 562 "help help"]

    describe "LogAnalysis.insert" $ do
        context "when the log is Unknown" $ do
            it "leaves the tree unchanged" $ do
                let tree = (Node Leaf (LogMessage Info 1 "bar") Leaf)
                insert (Unknown "foo") tree `shouldBe` tree

        context "when the tree is empty" $ do
            it "builds a one-leaf tree" $ do
                let message = LogMessage Info 1 "bar"
                insert message Leaf `shouldBe` Node Leaf message Leaf

        it "inserts a log into a sorted MessageTree" $ do
            let logAt1 = LogMessage Info 1 "foo"
            let logAt2 = LogMessage Info 2 "bar"
            let logAt3 = LogMessage Info 3 "baz"
            let tree = Node (Node Leaf logAt1 Leaf) logAt3 Leaf
            let expectedTree = Node (Node Leaf logAt1 (Node Leaf logAt2 Leaf)) logAt3 Leaf

            insert logAt2 tree `shouldBe` expectedTree

    describe "LogAnalysis.build" $ do
        it "build a MessageTree from a list of LogMessages" $ do
            let logAt1 = LogMessage Info 1 "foo"
            let logAt2 = LogMessage Info 2 "bar"
            let logAt3 = LogMessage Info 3 "baz"
            let unknownLog = Unknown "unknown"
            let logs = [unknownLog, logAt3, logAt1, logAt2]
            let expectedTree = Node (Node Leaf logAt1 (Node Leaf logAt2 Leaf)) logAt3 Leaf

            build logs `shouldBe` expectedTree

    describe "LogAnalysis.inOrder" $ do
        context "then the tree is empty" $ do
            it "returns an empty list" $ do
                inOrder Leaf `shouldBe` []

        it "builds an ordered list of LogMessages from a MessageTree" $ do
            let logAt1 = LogMessage Info 1 "foo"
            let logAt2 = LogMessage Info 2 "bar"
            let logAt3 = LogMessage Info 3 "baz"
            let tree = Node (Node Leaf logAt1 (Node Leaf logAt2 Leaf)) logAt3 Leaf

            inOrder tree `shouldBe` [logAt1, logAt2, logAt3]

    describe "LogAnalysis.whatWentWrong" $ do
        it "extracts the error messages with severity >= 50, sorted by timestamp" $ do
            let logs = [LogMessage Info 6 "Completed armadillo processing",
                        LogMessage Info 1 "Nothing to report",
                        LogMessage (Error 99) 10 "Flange failed!",
                        LogMessage Info 4 "Everything normal",
                        LogMessage Info 11 "Initiating self-destruct sequence",
                        LogMessage (Error 70) 3 "Way too many pickles",
                        LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
                        LogMessage Warning 5 "Flange is due for a check-up",
                        LogMessage Info 7 "Out for lunch, back in two time steps",
                        LogMessage (Error 20) 2 "Too many pickles",
                        LogMessage Info 9 "Back from lunch"]

            whatWentWrong logs `shouldBe` ["Way too many pickles",
                                           "Bad pickle-flange interaction detected",
                                           "Flange failed!"]
