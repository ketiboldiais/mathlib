import { getLatestPostSummaries } from "@/utils/mdxUtils";
import Link from "next/link";

const posts = await getLatestPostSummaries();

export default function Home() {
  return (
    <div>
      <main>
        <h2>Winnow</h2>
      </main>
      <div>
        <p>
          Winnow is a scripting language designed for numerical analysis,
          symbolic computation, and statistical analysis, written in
          TypeScript. The pages below provide the language's documentation.
        </p>
        <ul>
          {posts.map((post) => (
            <li key={post.slug} className="postlink">
              <Link href={`/blog/${post.slug}`}>
                <h2>{post.title}</h2>
                <p>{post.description}</p>
                <p>Last updated: {new Date(post.date).toDateString()}</p>
              </Link>
            </li>
          ))}
        </ul>
      </div>
      <footer className="row-start-3 flex gap-6 flex-wrap items-center justify-center"></footer>
    </div>
  );
}
