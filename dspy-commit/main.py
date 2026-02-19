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
        """Create a concise, descriptive commit message (80 characters or less) and if necessary a body. Include the type of change (feat, fix, docs, etc.) and a brief description"""

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
