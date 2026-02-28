import argparse
import os

import dspy


def main():
    parser = argparse.ArgumentParser(
        description="Generate a commit message from a git diff"
    )
    parser.add_argument("input_file", help="Path to file containing the git diff")
    parser.add_argument(
        "output_file", help="Path to write the generated commit message"
    )
    args = parser.parse_args()

    # Read the git diff from input file
    with open(args.input_file, "r") as f:
        git_diff = f.read()

    lm = dspy.LM(
        model="openrouter/mistralai/mistral-small-3.2-24b-instruct",
        api_key=os.environ["OPENROUTER_API_KEY"],
        api_base="https://openrouter.ai/api/v1/chat/completions",
    )
    dspy.configure(lm=lm)

    class CommitSignature(dspy.Signature):
        """Summarize changes in around 50 characters or less

        More detailed explanatory text, if necessary. Wrap it to about 72
        characters or so. In some contexts, the first line is treated as the
        subject of the commit and the rest of the text as the body. The
        blank line separating the summary from the body is critical (unless
        you omit the body entirely); various tools like `log`, `shortlog`
        and `rebase` can get confused if you run the two together.

        Explain the problem that this commit is solving. Focus on why you
        are making this change as opposed to how (the code explains that).
        Are there side effects or other unintuitive consequences of this
        change? Here's the place to explain them.

        Further paragraphs come after blank lines.

         - Bullet points are okay, too

         - Typically a hyphen or asterisk is used for the bullet, preceded
           by a single space, with blank lines in between, but conventions
           vary here

        Include the type of change (feat, fix, docs, etc.) and a brief description
        """

        # from https://chris.beams.io/git-commit, extended with type of change

        git_diff: str = dspy.InputField()
        commit_msg: str = dspy.OutputField()

    agent = dspy.Predict(CommitSignature)

    result = agent(git_diff=git_diff)

    # Write the commit message to output file
    with open(args.output_file, "w") as f:
        f.write(result.commit_msg)

    print(f"Commit message written to {args.output_file}")


if __name__ == "__main__":
    main()
