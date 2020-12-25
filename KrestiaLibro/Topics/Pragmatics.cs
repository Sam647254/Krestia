namespace KrestiaLibro.Topics {
   public static partial class Topics {
      public static Topic Pragmatics = Topic.Create("Krestia's pragmatics",
         b => {
            b.Paragraph(p => {
               p.Text(@"
The vocabulary and grammar elements of Krestia are geared towards one central
goal of communication: the exchange of information. While its design attempts
to make utterances sound natural, it ultimately focuses on serializing the
speaker’s thoughts as cleanly as possible, to be interpreted by the listener
without any loss of meaning. Because of this focus, Krestia’s pragmatics have
the following characteristics, which may significantly differ from those of
natural languages:");
            });
            b.Ul(
               b.Li(b.Strong(b.T("No expressions of politeness. ")),
                  b.T(@"
Such expressions, while pleasing to the ear and the ego, do not actually
contribute to the core meaning of an utterance. While this takes away the
ability to show manners and respect, it also eliminates the burden, especially
on language learners, of having to decide how to phrase a request that will
minimize the possibility of offending the listener.")),
               b.Li(
                  b.Strong(b.T("Literal interpretations of all words. ")),
                  b.T(@"
As idiomatic expressions vary from culture to culture, they become a point of
confusion when people of different backgrounds communicate with each other. To
avoid misunderstanding that arises from metaphoric use of words, all expressions
are interpreted by their denotations, unless used in an artistic context, such
as poems or lyrics.")),
               b.Li(b.Strong(b.T("Well-defined rules to providing responses. ")),
                  b.T(@"
A speaker who wishes to seek information from a listener may do so by issuing a
request using a “response-seeking verb”, and the listener will have a finite number
of ways to respond that may be considered acceptable. Thus, if the listener fails
to use one of these valid responses, the asker can explicitly point out that the
listener did not answer the question. In all cases, the listener will also be able
to refuse to answer or indicate a lack of sufficient knowledge to answer.")),
               b.Li(b.Strong(b.T("Objectiveness in modifier words")),
                  b.T(@"
Descriptors that imply biased preferences will be explicitly marked in the
dictionary to indicate that they reflect personal preferences. For example, one
person’s “tasty” food may not be the same as any other person’s perception of
“tasty” food.")));
         });
   }
}