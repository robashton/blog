I [mentioned in the last entry](/entries/evented-github-adventure---crossing-the-streams-to-gain-real-insights.html) that these "paranoid pushes" might actually represent mistakes, but how might we go about measuring this?

Well, actually - let's do something slightly different to that, and measure "sentiment", and see what happens.

To do this, I downloaded a list of words for both positive and negative sentiment and ran the following projection over my events:


    function collectHappinessIndexOfCommit(commit, state) {
       var index = 0
       for(var i in happyWords) {
           if(commit.message.indexOf(happyWords[i]) >= 0)
              state.happycount++
       }
       for(var i in sadWords) {
           if(commit.message.indexOf(sadWords[i]) >= 0)
              state.sadcount++
       }
       state.commits++
    }

    function collectHappinessIndexOfPush(pushEvent, state) {
      for(var i =0 ; i < pushEvent.body.payload.commits.length; i++) {
        var commit = pushEvent.body.payload.commits[i]
        collectHappinessIndexOfCommit(commit, state)
      }
    }

    fromStreams(['github', 'github-paranoidpushes'])
      .when({
        "$init": function() {
          return { 
              paranoid: {
                 commits: 0, sadcount: 0, happycount: 0
              },
              all: {
                 commits: 0, sadcount: 0, happycount: 0
              }
          }
        },
        "PushEvent": function(state, ev) {
           collectHappinessIndexOfPush(ev, state.all)
        },
        "ParanoidPush": function(state, ev) {
            collectHappinessIndexOfPush(ev.body.next, state.paranoid)
        }
      })

Pretty simple, although not the most "complete" way to measure sentiment, it should give some indication as to what is going on with our messages, PS, I have to cite a paper at this point because I agreed to do so when I downloaded that list of words so


       Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews."
           Proceedings of the ACM SIGKDD International Conference on Knowledge
           Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle,
           Washington, USA, 
       Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing
           and Comparing Opinions on the Web." Proceedings of the 14th
           International World Wide Web conference (WWW-2005), May 10-14,
           2005, Chiba, Japan.


Anyway, what do we get?


